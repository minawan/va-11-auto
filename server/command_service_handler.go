package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
	"strings"
	"time"
)

type CommandServiceHandler struct {
	redisClient       *redis.Client
	nextTransactionId int64
}

type commandSpec struct {
	transactionId int64
	useShortcut   bool
}

func NewCommandServiceHandler(redisClient *redis.Client) command.CommandService {
	return &CommandServiceHandler{
		redisClient:       redisClient,
		nextTransactionId: 0,
	}
}

func (handler *CommandServiceHandler) getNextTransactionId() int64 {
	transactionId := handler.nextTransactionId
	handler.nextTransactionId++
	if handler.nextTransactionId > 30000 {
		handler.nextTransactionId = 0
	}
	return transactionId
}

func (handler *CommandServiceHandler) find(name shared.ScreenElementType) (*ScreenElement, error) {
	var screenElement ScreenElement
	screenElementMap, err := handler.redisClient.HGetAll("element:" + name.String()).Result()
	if err != nil {
		return nil, err
	}

	if xCoordValue, ok := screenElementMap["x_coord"]; ok {
		xCoord, err := strconv.ParseInt(xCoordValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.XCoord = int32(xCoord)
	}
	if yCoordValue, ok := screenElementMap["y_coord"]; ok {
		yCoord, err := strconv.ParseInt(yCoordValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.YCoord = int32(yCoord)
	}
	if shortcutValue, ok := screenElementMap["shortcut"]; ok {
		shortcut, err := strconv.ParseInt(shortcutValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.Shortcut = int32(shortcut)
	}

	return &screenElement, nil
}

func (handler *CommandServiceHandler) loadRecipeActions(transactionId int64) ([]*action.RecipeAction, error) {
	recipeActions := []*action.RecipeAction{}
	key := fmt.Sprintf("actions:%d", transactionId)
	length := handler.redisClient.LLen(key).Val()
	recipeActionValues := handler.redisClient.LRange(key, 0, length-1).Val()
	for i := length - 1; i >= 0; i-- {
		tokens := strings.SplitN(recipeActionValues[i], " ", 3)
		if len(tokens) < 1 {
			continue
		}
		ra := action.NewRecipeAction()
		switch tokens[0] {
		case "RESET":
			reset := action.NewResetAction()
			ra.ResetAction = reset
		case "SELECT_SLOT":
			if len(tokens) < 2 {
				continue
			}
			slot, err := strconv.ParseInt(tokens[1], 10, 64)
			if err != nil {
				continue
			}
			selectSlot := action.NewSelectSlotAction()
			selectSlot.Name = shared.ScreenElementType(slot)
			ra.SelectSlotAction = selectSlot
		case "ADD_INGREDIENT":
			if len(tokens) < 3 {
				continue
			}
			ingredient, err := strconv.ParseInt(tokens[1], 10, 64)
			if err != nil {
				continue
			}
			quantity, err := strconv.ParseInt(tokens[2], 10, 32)
			if err != nil {
				continue
			}
			addIngredient := action.NewAddIngredientAction()
			addIngredient.Name = shared.ScreenElementType(ingredient)
			addIngredient.Quantity = int32(quantity)
			ra.AddIngredientAction = addIngredient
		case "ADD_ICE":
			addIce := action.NewAddIceAction()
			ra.AddIceAction = addIce
		case "AGE":
			age := action.NewAgeAction()
			ra.AgeAction = age
		case "MIX":
			if len(tokens) < 2 {
				continue
			}
			durationInSeconds, err := strconv.ParseInt(tokens[1], 10, 32)
			if err != nil {
				continue
			}
			mix := action.NewMixAction()
			mix.DurationInSeconds = int32(durationInSeconds)
			ra.MixAction = mix
		case "SERVE":
			serve := action.NewServeAction()
			ra.ServeAction = serve
		}
		recipeActions = append(recipeActions, ra)
	}
	return recipeActions, nil
}

func (handler *CommandServiceHandler) receiveRecipeActions(actionsQueue <-chan *redis.Message, transactionId int64) (*commandSpec, error) {
	select {
	case msg := <-actionsQueue:
		fmt.Println(msg.Channel, msg.Payload)
		numTokens := 2
		tokens := strings.SplitN(msg.Payload, " ", numTokens)
		if len(tokens) < numTokens {
			return nil, fmt.Errorf("Invalid number of tokens for a message in %s: %s - Expected: %d, Received: %d", msg.Channel, msg.Payload, numTokens, len(tokens))
		}
		transactionId, err := strconv.ParseInt(tokens[0], 10, 64)
		if err != nil {
			return nil, err
		}
		useShortcut, err := strconv.ParseBool(tokens[1])
		if err != nil {
			return nil, err
		}
		return &commandSpec{transactionId: transactionId, useShortcut: useShortcut}, nil
	case <-time.After(3 * time.Second):
		return nil, fmt.Errorf("Transaction %d timed out.", transactionId)
	}
}

func (handler *CommandServiceHandler) GetCommands(ctx context.Context, drinkName recipe.DrinkName, addKarmotrine bool, bigSize bool, reset bool, slot shared.ScreenElementType, serve bool, useShortcut bool) ([]*command.Command, error) {
	transactionId := handler.getNextTransactionId()
	actionsQueue := handler.redisClient.Subscribe(fmt.Sprintf("actions.queue.%d", transactionId)).Channel()
	err := handler.redisClient.Publish("request.queue", fmt.Sprintf("%d %d %s %s %s %d %s %s", transactionId, drinkName, strconv.FormatBool(addKarmotrine), strconv.FormatBool(bigSize), strconv.FormatBool(reset), slot, strconv.FormatBool(serve), strconv.FormatBool(useShortcut))).Err()
	if err != nil {
		return nil, err
	}
	spec, err := handler.receiveRecipeActions(actionsQueue, transactionId)
	if err != nil {
		fmt.Println(err)
		return nil, err
	}

	transactionId = spec.transactionId
	useShortcut = spec.useShortcut

	recipeActions, err := handler.loadRecipeActions(transactionId)
	if err != nil {
		return nil, err
	}

	commands := []*command.Command{}

	for _, recipeAction := range recipeActions {
		if recipeAction.ResetAction != nil {
			reset, err := handler.find(shared.ScreenElementType_RESET)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(reset, useShortcut))
		} else if recipeAction.SelectSlotAction != nil {
			slot, err := handler.find(recipeAction.SelectSlotAction.Name)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(slot, useShortcut))
		} else if recipeAction.AddIngredientAction != nil {
			source, err := handler.find(recipeAction.AddIngredientAction.Name)
			if err != nil {
				return nil, err
			}
			destination, err := handler.find(shared.ScreenElementType_BLENDER)
			if err != nil {
				return nil, err
			}
			for i := 0; i < int(recipeAction.AddIngredientAction.Quantity); i++ {
				cmd := command.NewCommand()
				shortcut := source.Shortcut
				if useShortcut && shortcut != 0 {
					cmd.TypeCommand = command.NewTypeCommand()
					cmd.TypeCommand.Key = int8(string(shortcut)[0])
					commands = append(commands, cmd)
					continue
				}
				cmd.DragAndDropCommand = command.NewDragAndDropCommand()
				srcPos := shared.NewCoord()
				srcPos.X = source.XCoord
				srcPos.Y = source.YCoord
				dstPos := shared.NewCoord()
				dstPos.X = destination.XCoord
				dstPos.Y = destination.YCoord
				cmd.DragAndDropCommand.Source = srcPos
				cmd.DragAndDropCommand.Destination = dstPos
				commands = append(commands, cmd)
			}
		} else if recipeAction.AddIceAction != nil {
			addIce, err := handler.find(shared.ScreenElementType_ADD_ICE)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(addIce, useShortcut))
		} else if recipeAction.AgeAction != nil {
			age, err := handler.find(shared.ScreenElementType_AGE)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(age, useShortcut))
		} else if recipeAction.MixAction != nil {
			mix, err := handler.find(shared.ScreenElementType_MIX)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(mix, useShortcut))
			cmd := command.NewCommand()
			cmd.WaitCommand = command.NewWaitCommand()
			cmd.WaitCommand.DurationInSeconds = recipeAction.MixAction.DurationInSeconds
			commands = append(commands, cmd)
			commands = append(commands, trigger(mix, useShortcut))
		} else if recipeAction.ServeAction != nil {
			mix, err := handler.find(shared.ScreenElementType_MIX)
			if err != nil {
				return nil, err
			}
			commands = append(commands, trigger(mix, useShortcut))
		} else {
			return nil, errors.New(fmt.Sprintf("Unexpected Action type: %#v", recipeAction))
		}
	}

	fmt.Println(commands)
	return commands, nil
}

func trigger(screenElement *ScreenElement, useShortcut bool) *command.Command {
	cmd := command.NewCommand()

	shortcut := screenElement.Shortcut
	if useShortcut && shortcut != 0 {
		cmd.TypeCommand = command.NewTypeCommand()
		cmd.TypeCommand.Key = int8(string(shortcut)[0])
		return cmd
	}

	cmd.ClickCommand = command.NewClickCommand()
	pos := shared.NewCoord()
	pos.X = screenElement.XCoord
	pos.Y = screenElement.YCoord
	cmd.ClickCommand.Position = pos

	return cmd
}
