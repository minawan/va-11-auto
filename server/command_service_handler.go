package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
)

type CommandServiceHandler struct{
	RedisClient *redis.Client
}

func NewCommandServiceHandler(redisClient *redis.Client) command.CommandService {
	return &CommandServiceHandler{RedisClient: redisClient}
}

func (handler *CommandServiceHandler) Find(name shared.ScreenElementType) (*ScreenElement, error) {
	var screenElement ScreenElement
	screenElementMap, err := handler.RedisClient.HGetAll("element:" + name.String()).Result()
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

func (handler *CommandServiceHandler) GetCommands(ctx context.Context, recipeAction *action.RecipeAction, useShortcut bool) ([]*command.Command, error) {
	fmt.Println(recipeAction)
	commands := []*command.Command{}

	if recipeAction.ResetAction != nil {
		reset, err := handler.Find(shared.ScreenElementType_RESET)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(reset, useShortcut))
	} else if recipeAction.SelectSlotAction != nil {
		slot, err := handler.Find(recipeAction.SelectSlotAction.Slot)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(slot, useShortcut))
	} else if recipeAction.AddIngredientAction != nil {
		source, err := handler.Find(recipeAction.AddIngredientAction.Ingredient)
		if err != nil {
			return nil, err
		}
		destination, err := handler.Find(shared.ScreenElementType_BLENDER)
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
		addIce, err := handler.Find(shared.ScreenElementType_ADD_ICE)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(addIce, useShortcut))
	} else if recipeAction.AgeAction != nil {
		age, err := handler.Find(shared.ScreenElementType_AGE)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(age, useShortcut))
	} else if recipeAction.MixAction != nil {
		mix, err := handler.Find(shared.ScreenElementType_MIX)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(mix, useShortcut))
		cmd := command.NewCommand()
		cmd.WaitCommand = command.NewWaitCommand()
		cmd.WaitCommand.DurationInSeconds = recipeAction.MixAction.DurationInSeconds
		commands = append(commands, cmd)
		commands = append(commands, Trigger(mix, useShortcut))
	} else if recipeAction.ServeAction != nil {
		mix, err := handler.Find(shared.ScreenElementType_MIX)
		if err != nil {
			return nil, err
		}
		commands = append(commands, Trigger(mix, useShortcut))
	} else {
		return nil, errors.New(fmt.Sprintf("Unexpected Action type: %#v", recipeAction))
	}

	fmt.Println(commands)
	return commands, nil
}

func Trigger(screenElement *ScreenElement, useShortcut bool) *command.Command {
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
