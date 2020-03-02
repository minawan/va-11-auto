package main

import (
	"context"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
)

type RecipeActionServiceHandler struct{}

func NewRecipeActionServiceHandler() *RecipeActionServiceHandler {
	return &RecipeActionServiceHandler{}
}

func (*RecipeActionServiceHandler) GetRecipeActions(ctx context.Context, request *action.RecipeActionRequest) (*action.RecipeActionResponse, error) {
	fmt.Println(request)
	var actions []*action.RecipeAction

	if request.Reset {
		resetAction := action.NewRecipeAction()
		resetAction.ResetAction = action.NewResetAction()
		actions = append(actions, resetAction)
	}

	selectSlotAction := action.NewRecipeAction()
	selectSlotAction.SelectSlotAction = action.NewSelectSlotAction()
	selectSlotAction.SelectSlotAction.Slot = request.Slot
	actions = append(actions, selectSlotAction)

	drinkRecipe := request.DrinkRecipe
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

	if request.Serve {
		serveAction := action.NewRecipeAction()
		serveAction.ServeAction = action.NewServeAction()
		actions = append(actions, serveAction)
	}

	response := action.RecipeActionResponse{Actions: actions}
	fmt.Println(response)
	return &response, nil
}
