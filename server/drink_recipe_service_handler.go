package main

import (
	"context"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
)

type DrinkRecipeServiceHandler struct{}

func NewDrinkRecipeServiceHandler() *DrinkRecipeServiceHandler {
	return &DrinkRecipeServiceHandler{}
}

func (*DrinkRecipeServiceHandler) GetDrinkRecipe(ctx context.Context, request *recipe.DrinkRecipeRequest) (*recipe.DrinkRecipeResponse, error) {
	return &recipe.DrinkRecipeResponse{}, nil
}
