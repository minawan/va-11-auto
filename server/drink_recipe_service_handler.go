package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type DrinkRecipeServiceHandler struct {
	Recipes *[]DrinkRecipe
}

func NewDrinkRecipeServiceHandler(recipes *[]DrinkRecipe) recipe.DrinkRecipeService {
	return &DrinkRecipeServiceHandler{Recipes: recipes}
}

func (handler *DrinkRecipeServiceHandler) GetDrinkRecipe(ctx context.Context, drinkName recipe.DrinkName, addKarmotrine bool, bigSize bool) (*recipe.DrinkRecipe, error) {
	fmt.Println(drinkName.String())
	if bigSize && drinkName == recipe.DrinkName_CREVICE_SPIKE {
		addKarmotrine = true
	}

	response := recipe.NewDrinkRecipe()
	for _, drinkRecipe := range *handler.Recipes {
		if drinkRecipe.Name != drinkName.String() {
			continue
		}
		recipeInfo := drinkRecipe.Recipe

		adelhyde := recipeInfo.Adelhyde
		bronsonExtract := recipeInfo.BronsonExtract
		powderedDelta := recipeInfo.PowderedDelta
		flanergide := recipeInfo.Flanergide
		karmotrine := recipeInfo.Karmotrine

		if bigSize && !drinkRecipe.IsBig() {
			adelhyde *= 2
			bronsonExtract *= 2
			powderedDelta *= 2
			flanergide *= 2
			karmotrine *= 2
		}

		if karmotrine < 0 && addKarmotrine {
			karmotrine = 1
		}

		response.Quantity = make(map[shared.ScreenElementType]int32)
		response.Quantity[shared.ScreenElementType_ADELHYDE] = adelhyde
		response.Quantity[shared.ScreenElementType_BRONSON_EXTRACT] = bronsonExtract
		response.Quantity[shared.ScreenElementType_POWDERED_DELTA] = powderedDelta
		response.Quantity[shared.ScreenElementType_FLANERGIDE] = flanergide
		response.Quantity[shared.ScreenElementType_KARMOTRINE] = karmotrine

		response.AddIce = recipeInfo.AddIce
		response.Age = recipeInfo.Age
		response.Blend = recipeInfo.Wait

		fmt.Println(*response)
		return response, nil
	}

	return nil, errors.New(fmt.Sprintf("Recipe for %s not found!", drinkName.String()))
}
