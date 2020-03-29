package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
)

type RecipeActionServiceHandler struct {
	RedisClient       *redis.Client
	NextTransactionId int32
}

func NewRecipeActionServiceHandler(redisClient *redis.Client) action.RecipeActionService {
	return &RecipeActionServiceHandler{RedisClient: redisClient, NextTransactionId: 0}
}

func (handler *RecipeActionServiceHandler) getNextTransactionId() int32 {
	transactionId := handler.NextTransactionId
	handler.NextTransactionId++
	if handler.NextTransactionId > 30000 {
		handler.NextTransactionId = 0
	}
	return transactionId
}

func (handler *RecipeActionServiceHandler) emitResetAction(key string) {
	handler.RedisClient.LPush(key, "RESET")
}

func (handler *RecipeActionServiceHandler) emitSelectSlotAction(key string, slot shared.ScreenElementType) {
	handler.RedisClient.LPush(key, fmt.Sprintf("SELECT_SLOT %d", slot))
}

func (handler *RecipeActionServiceHandler) emitAddIngredientAction(key string, ingredient shared.ScreenElementType, quantity int32) {
	handler.RedisClient.LPush(key, fmt.Sprintf("ADD_INGREDIENT %d %d", ingredient, quantity))
}

func (handler *RecipeActionServiceHandler) emitAddIceAction(key string) {
	handler.RedisClient.LPush(key, "ADD_ICE")
}

func (handler *RecipeActionServiceHandler) emitAgeAction(key string) {
	handler.RedisClient.LPush(key, "AGE")
}

func (handler *RecipeActionServiceHandler) emitMixAction(key string, blend bool) {
	durationInSeconds := 1
	if blend {
		durationInSeconds = 5
	}
	handler.RedisClient.LPush(key, fmt.Sprintf("MIX %d", durationInSeconds))
}

func (handler *RecipeActionServiceHandler) emitServeAction(key string) {
	handler.RedisClient.LPush(key, "SERVE")
}

func getIngredientFromDrinkRecipeMap(drinkRecipeMap map[string]string, ingredientName string) (int32, error) {
	if ingredientValue, ok := drinkRecipeMap[ingredientName]; ok {
		ingredientCount, err := strconv.ParseInt(ingredientValue, 10, 32)
		if err != nil {
			return 0, err
		}
		return int32(ingredientCount), nil
	}
	return 0, errors.New(fmt.Sprintf("Ingredient %s not found.", ingredientName))
}

func getOptionFromDrinkRecipeMap(drinkRecipeMap map[string]string, optionName string) (bool, error) {
	if optionValue, ok := drinkRecipeMap[optionName]; ok {
		return strconv.ParseBool(optionValue)
	}
	return false, errors.New(fmt.Sprintf("Option %s not found.", optionName))
}

func (handler *RecipeActionServiceHandler) loadDrinkRecipe(transactionId int32) (drinkRecipe *recipe.DrinkRecipe, err error) {
	drinkRecipe = recipe.NewDrinkRecipe()
	drinkRecipe.Quantity = make(map[shared.ScreenElementType]int32)
	key := fmt.Sprintf("recipe:%d", transactionId)
	drinkRecipeMap := handler.RedisClient.HGetAll(key).Val()
	drinkRecipe.Quantity[shared.ScreenElementType_ADELHYDE], err = getIngredientFromDrinkRecipeMap(drinkRecipeMap, "adelhyde")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Quantity[shared.ScreenElementType_BRONSON_EXTRACT], err = getIngredientFromDrinkRecipeMap(drinkRecipeMap, "bronson_extract")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Quantity[shared.ScreenElementType_POWDERED_DELTA], err = getIngredientFromDrinkRecipeMap(drinkRecipeMap, "powdered_delta")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Quantity[shared.ScreenElementType_FLANERGIDE], err = getIngredientFromDrinkRecipeMap(drinkRecipeMap, "flanergide")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Quantity[shared.ScreenElementType_KARMOTRINE], err = getIngredientFromDrinkRecipeMap(drinkRecipeMap, "karmotrine")
	if err != nil {
		return nil, err
	}
	drinkRecipe.AddIce, err = getOptionFromDrinkRecipeMap(drinkRecipeMap, "add_ice")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Age, err = getOptionFromDrinkRecipeMap(drinkRecipeMap, "age")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Blend, err = getOptionFromDrinkRecipeMap(drinkRecipeMap, "wait")
	if err != nil {
		return nil, err
	}
	return drinkRecipe, nil
}

func (handler *RecipeActionServiceHandler) GetRecipeActions(ctx context.Context, drinkRecipeTransactionId int32, reset bool, slot shared.ScreenElementType, serve bool) (int32, error) {
	recipeActionTransactionId := handler.getNextTransactionId()
	key := fmt.Sprintf("actions:%d", recipeActionTransactionId)
	drinkRecipe, err := handler.loadDrinkRecipe(drinkRecipeTransactionId)
	if err != nil {
		fmt.Println(err)
		return 0, err
	}
	fmt.Println(drinkRecipe)

	if reset {
		handler.emitResetAction(key)
	}

	handler.emitSelectSlotAction(key, slot)

	for ingredient, quantity := range drinkRecipe.Quantity {
		handler.emitAddIngredientAction(key, ingredient, quantity)
	}

	if drinkRecipe.AddIce {
		handler.emitAddIceAction(key)
	}

	if drinkRecipe.Age {
		handler.emitAgeAction(key)
	}

	handler.emitMixAction(key, drinkRecipe.Blend)

	if serve {
		handler.emitServeAction(key)
	}

	return recipeActionTransactionId, nil
}
