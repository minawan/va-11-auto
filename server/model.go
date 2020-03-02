package main

type RecipeInfo struct {
	Adelhyde       int32
	BronsonExtract int32
	PowderedDelta  int32
	Flanergide     int32
	Karmotrine     int32
	AddIce         bool
	Age            bool
	Wait           bool
}

type DrinkRecipe struct {
	Name   string
	Flavor string
	Kind   string
	Trait  string
	Price  int
	Recipe RecipeInfo
}

func (drinkRecipe *DrinkRecipe) IsBig() bool {
	recipeInfo := drinkRecipe.Recipe
	acc := recipeInfo.Adelhyde
	acc += recipeInfo.BronsonExtract
	acc += recipeInfo.PowderedDelta
	acc += recipeInfo.Flanergide
	acc += recipeInfo.Karmotrine
	return acc > 10
}
