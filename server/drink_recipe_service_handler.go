package main

import (
	"context"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
)

type DrinkRecipeServiceHandler struct {
	RedisClient *redis.Client
}

func NewDrinkRecipeServiceHandler(redisClient *redis.Client) recipe.DrinkRecipeService {
	return &DrinkRecipeServiceHandler{RedisClient: redisClient}
}

func (handler *DrinkRecipeServiceHandler) Find(name recipe.DrinkName) (*DrinkRecipe, error) {
	var drinkRecipe DrinkRecipe
	drinkRecipeMap, err := handler.RedisClient.HGetAll("recipe:" + name.String()).Result()
	if err != nil {
		return nil, err
	}

	if adelhydeValue, ok := drinkRecipeMap["adelhyde"]; ok {
		adelhyde, err := strconv.ParseInt(adelhydeValue, 10, 32)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.Adelhyde = int32(adelhyde)
	}
	if bronsonExtractValue, ok := drinkRecipeMap["bronson_extract"]; ok {
		bronsonExtract, err := strconv.ParseInt(bronsonExtractValue, 10, 32)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.BronsonExtract = int32(bronsonExtract)
	}
	if powderedDeltaValue, ok := drinkRecipeMap["powdered_delta"]; ok {
		powderedDelta, err := strconv.ParseInt(powderedDeltaValue, 10, 32)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.PowderedDelta = int32(powderedDelta)
	}
	if flanergideValue, ok := drinkRecipeMap["flanergide"]; ok {
		flanergide, err := strconv.ParseInt(flanergideValue, 10, 32)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.Flanergide = int32(flanergide)
	}
	if karmotrineValue, ok := drinkRecipeMap["karmotrine"]; ok {
		karmotrine, err := strconv.ParseInt(karmotrineValue, 10, 32)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.Karmotrine = int32(karmotrine)
	}
	if addIceValue, ok := drinkRecipeMap["add_ice"]; ok {
		addIce, err := strconv.ParseBool(addIceValue)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.AddIce = addIce
	}
	if ageValue, ok := drinkRecipeMap["age"]; ok {
		age, err := strconv.ParseBool(ageValue)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.Age = age
	}
	if waitValue, ok := drinkRecipeMap["wait"]; ok {
		wait, err := strconv.ParseBool(waitValue)
		if err != nil {
			return nil, err
		}
		drinkRecipe.Recipe.Wait = wait
	}

	return &drinkRecipe, nil
}

func (handler *DrinkRecipeServiceHandler) GetDrinkRecipe(ctx context.Context, drinkName recipe.DrinkName, addKarmotrine bool, bigSize bool) (*recipe.DrinkRecipe, error) {
	fmt.Println(drinkName.String())
	if bigSize && drinkName == recipe.DrinkName_CREVICE_SPIKE {
		addKarmotrine = true
	}

	drinkRecipe, err := handler.Find(drinkName)
	if err != nil {
		return nil, err
	}

	response := recipe.NewDrinkRecipe()
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
