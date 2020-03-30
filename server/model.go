package main

type DrinkRecipe struct {
	Adelhyde       int64
	BronsonExtract int64
	PowderedDelta  int64
	Flanergide     int64
	Karmotrine     int64
	AddIce         bool
	Age            bool
	Wait           bool
}

type ScreenElement struct {
	Name     string
	XCoord   int32
	YCoord   int32
	Shortcut int32
}

func (drinkRecipe *DrinkRecipe) IsBig() bool {
	acc := drinkRecipe.Adelhyde
	acc += drinkRecipe.BronsonExtract
	acc += drinkRecipe.PowderedDelta
	acc += drinkRecipe.Flanergide
	acc += drinkRecipe.Karmotrine
	return acc > 10
}
