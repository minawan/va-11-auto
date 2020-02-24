package main

import (
	"context"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
)

type RecipeActionServerHandler struct { }

func NewRecipeActionServerHandler() *RecipeActionServerHandler {
	return &RecipeActionServerHandler{ }
}

func (*RecipeActionServerHandler) GetRecipeActions(ctx context.Context, request *action.RecipeActionRequest) (*action.RecipeActionResponse, error) {
	var actions []*action.RecipeAction

	if *request.Reset {
		resetAction := action.NewRecipeAction()
		resetAction.ResetAction = action.NewResetAction()
		actions = append(actions, resetAction)
	}

	selectSlotAction := action.NewRecipeAction()
	selectSlotAction.SelectSlotAction = action.NewSelectSlotAction()
	selectSlotAction.SelectSlotAction.Slot = request.Slot
	actions = append(actions, selectSlotAction)

	recipeIngredients := []action.ScreenElementType{
		action.ScreenElementType_ADELHYDE,
		action.ScreenElementType_BRONSON_EXTRACT,
		action.ScreenElementType_POWDERED_DELTA,
		action.ScreenElementType_FLANERGIDE,
		action.ScreenElementType_KARMOTRINE,
	}
	drinkRecipe := request.DrinkRecipe
	for _, ingredient := range recipeIngredients {
		addIngredientAction := action.NewRecipeAction()
		addIngredientAction.AddIngredientAction = action.NewAddIngredientAction()
		addIngredientAction.AddIngredientAction.Ingredient = new(action.ScreenElementType)
		*addIngredientAction.AddIngredientAction.Ingredient = ingredient
		switch ingredient {
		case action.ScreenElementType_ADELHYDE:
			addIngredientAction.AddIngredientAction.Amount = drinkRecipe.Adelhyde
		case action.ScreenElementType_BRONSON_EXTRACT:
			addIngredientAction.AddIngredientAction.Amount = drinkRecipe.BronsonExtract
		case action.ScreenElementType_POWDERED_DELTA:
			addIngredientAction.AddIngredientAction.Amount = drinkRecipe.PowderedDelta
		case action.ScreenElementType_FLANERGIDE:
			addIngredientAction.AddIngredientAction.Amount = drinkRecipe.Flanergide
		case action.ScreenElementType_KARMOTRINE:
			addIngredientAction.AddIngredientAction.Amount = drinkRecipe.Karmotrine
		default:
			fmt.Println("Unexpected ScreenElementType:", ingredient)
		}
		actions = append(actions, addIngredientAction)
	}

	if *drinkRecipe.AddIce {
		addIceAction := action.NewRecipeAction()
		addIceAction.AddIceAction = action.NewAddIceAction()
		actions = append(actions, addIceAction)
	}

	if *drinkRecipe.Age {
		ageAction := action.NewRecipeAction()
		ageAction.AgeAction = action.NewAgeAction()
		actions = append(actions, ageAction)
	}

	mixAction := action.NewRecipeAction()
	mixAction.MixAction = action.NewMixAction()
	var durationInSeconds int32 = 1
	if *drinkRecipe.Blend {
		durationInSeconds = 5
	}
	mixAction.MixAction.DurationInSeconds = &durationInSeconds
	actions = append(actions, mixAction)

	if *request.Serve {
		serveAction := action.NewRecipeAction()
		serveAction.ServeAction = action.NewServeAction()
		actions = append(actions, serveAction)
	}

	return &action.RecipeActionResponse{Actions: actions}, nil
}
