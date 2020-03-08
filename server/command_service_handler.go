package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type CommandServiceHandler struct{}

func NewCommandServiceHandler() command.CommandService {
	return &CommandServiceHandler{}
}

func (*CommandServiceHandler) GetCommands(ctx context.Context, screenElements map[shared.ScreenElementType]*shared.ScreenElement, recipeAction *action.RecipeAction, useShortcut bool) ([]*command.Command, error) {
	fmt.Println(recipeAction)
	commands := []*command.Command{}

	if recipeAction.ResetAction != nil {
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_RESET], useShortcut))
	} else if recipeAction.SelectSlotAction != nil {
		commands = append(commands, Trigger(screenElements[recipeAction.SelectSlotAction.Slot], useShortcut))
	} else if recipeAction.AddIngredientAction != nil {
		source := screenElements[recipeAction.AddIngredientAction.Ingredient]
		destination := screenElements[shared.ScreenElementType_BLENDER]
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
			cmd.DragAndDropCommand.Source = source.Centroid
			cmd.DragAndDropCommand.Destination = destination.Centroid
			commands = append(commands, cmd)
		}
	} else if recipeAction.AddIceAction != nil {
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_ADD_ICE], useShortcut))
	} else if recipeAction.AgeAction != nil {
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_AGE], useShortcut))
	} else if recipeAction.MixAction != nil {
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_MIX], useShortcut))
		cmd := command.NewCommand()
		cmd.WaitCommand = command.NewWaitCommand()
		cmd.WaitCommand.DurationInSeconds = recipeAction.MixAction.DurationInSeconds
		commands = append(commands, cmd)
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_MIX], useShortcut))
	} else if recipeAction.ServeAction != nil {
		commands = append(commands, Trigger(screenElements[shared.ScreenElementType_MIX], useShortcut))
	} else {
		return nil, errors.New(fmt.Sprintf("Unexpected Action type: %#v", recipeAction))
	}

	fmt.Println(commands)
	return commands, nil
}

func Trigger(screenElement *shared.ScreenElement, useShortcut bool) *command.Command {
	cmd := command.NewCommand()
	shortcut := screenElement.Shortcut
	if useShortcut && shortcut != 0 {
		cmd.TypeCommand = command.NewTypeCommand()
		cmd.TypeCommand.Key = int8(string(shortcut)[0])
		return cmd
	}
	cmd.ClickCommand = command.NewClickCommand()
	cmd.ClickCommand.Position = screenElement.Centroid
	return cmd
}
