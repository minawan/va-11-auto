package main

import (
	"context"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type RecipeActionServiceHandler struct{}

func NewRecipeActionServiceHandler() action.RecipeActionService {
	return &RecipeActionServiceHandler{}
}

func (*RecipeActionServiceHandler) GetRecipeActions(ctx context.Context, drinkRecipe *recipe.DrinkRecipe, reset bool, slot shared.ScreenElementType, serve bool) ([]*action.RecipeAction, error) {
	fmt.Println(drinkRecipe)
	actions := []*action.RecipeAction{}

	if reset {
		resetAction := action.NewRecipeAction()
		resetAction.ResetAction = action.NewResetAction()
		actions = append(actions, resetAction)
	}

	selectSlotAction := action.NewRecipeAction()
	selectSlotAction.SelectSlotAction = action.NewSelectSlotAction()
	selectSlotAction.SelectSlotAction.Slot = slot
	actions = append(actions, selectSlotAction)

	for ingredient, quantity := range drinkRecipe.Quantity {
		addIngredientAction := action.NewRecipeAction()
		addIngredientAction.AddIngredientAction = action.NewAddIngredientAction()
		addIngredientAction.AddIngredientAction.Ingredient = ingredient
		addIngredientAction.AddIngredientAction.Quantity = quantity
		actions = append(actions, addIngredientAction)
	}

	if drinkRecipe.AddIce {
		addIceAction := action.NewRecipeAction()
		addIceAction.AddIceAction = action.NewAddIceAction()
		actions = append(actions, addIceAction)
	}

	if drinkRecipe.Age {
		ageAction := action.NewRecipeAction()
		ageAction.AgeAction = action.NewAgeAction()
		actions = append(actions, ageAction)
	}

	mixAction := action.NewRecipeAction()
	mixAction.MixAction = action.NewMixAction()
	mixAction.MixAction.DurationInSeconds = 1
	if drinkRecipe.Blend {
		mixAction.MixAction.DurationInSeconds = 5
	}
	actions = append(actions, mixAction)

	if serve {
		serveAction := action.NewRecipeAction()
		serveAction.ServeAction = action.NewServeAction()
		actions = append(actions, serveAction)
	}

	fmt.Println(actions)
	return actions, nil
}
