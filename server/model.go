package main

type RecipeInfo struct {
	Adelhyde       int64
	BronsonExtract int64
	PowderedDelta  int64
	Flanergide     int64
	Karmotrine     int64
	AddIce         bool
	Age            bool
	Wait           bool
}

type DrinkRecipe struct {
	Flavor string
	Kind   string
	Trait  string
	Price  int
	Recipe RecipeInfo
}

type ScreenElement struct {
	Name     string
	Category string
	XCoord   int32
	YCoord   int32
	Shortcut int32
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
