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

func NewDrinkRecipeServiceHandler(recipes *[]DrinkRecipe) *DrinkRecipeServiceHandler {
	return &DrinkRecipeServiceHandler{Recipes: recipes}
}

func (handler *DrinkRecipeServiceHandler) GetDrinkRecipe(ctx context.Context, request *recipe.DrinkRecipeRequest) (*recipe.DrinkRecipeResponse, error) {
	fmt.Println(request)

	if request.BigSize && request.DrinkName == recipe.DrinkName_CREVICE_SPIKE {
		request.AddKarmotrine = true
	}

	var response recipe.DrinkRecipeResponse
	for _, drinkRecipe := range *handler.Recipes {
		if drinkRecipe.Name != request.DrinkName.String() {
			continue
		}
		recipeInfo := drinkRecipe.Recipe

		adelhyde := recipeInfo.Adelhyde
		bronsonExtract := recipeInfo.BronsonExtract
		powderedDelta := recipeInfo.PowderedDelta
		flanergide := recipeInfo.Flanergide
		karmotrine := recipeInfo.Karmotrine

		if request.BigSize && !drinkRecipe.IsBig() {
			adelhyde *= 2
			bronsonExtract *= 2
			powderedDelta *= 2
			flanergide *= 2
			karmotrine *= 2
		}

		if request.AddKarmotrine {
			karmotrine++
		}

		response.DrinkRecipe = recipe.NewDrinkRecipe()
		response.DrinkRecipe.Quantity = make(map[shared.ScreenElementType]int32)
		response.DrinkRecipe.Quantity[shared.ScreenElementType_ADELHYDE] = adelhyde
		response.DrinkRecipe.Quantity[shared.ScreenElementType_BRONSON_EXTRACT] = bronsonExtract
		response.DrinkRecipe.Quantity[shared.ScreenElementType_POWDERED_DELTA] = powderedDelta
		response.DrinkRecipe.Quantity[shared.ScreenElementType_FLANERGIDE] = flanergide
		response.DrinkRecipe.Quantity[shared.ScreenElementType_KARMOTRINE] = karmotrine

		response.DrinkRecipe.AddIce = recipeInfo.AddIce
		response.DrinkRecipe.Age = recipeInfo.Age
		response.DrinkRecipe.Blend = recipeInfo.Wait

		fmt.Println(response)
		return &response, nil
	}

	return nil, errors.New(fmt.Sprintf("Recipe for %s not found!", request.DrinkName.String()))
}
