package main

import (
	"context"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type RecipeActionServiceHandler struct {
	RedisClient       *redis.Client
	NextTransactionId int32
}

func NewRecipeActionServiceHandler(redisClient *redis.Client) action.RecipeActionService {
	return &RecipeActionServiceHandler{RedisClient: redisClient, NextTransactionId: 0}
}

func (handler *RecipeActionServiceHandler) GetNextTransactionId() int32 {
	transactionId := handler.NextTransactionId
	handler.NextTransactionId++
	if handler.NextTransactionId > 30000 {
		handler.NextTransactionId = 0
	}
	return transactionId
}

func (handler *RecipeActionServiceHandler) EmitResetAction(key string) {
	handler.RedisClient.LPush(key, "RESET")
}

func (handler *RecipeActionServiceHandler) EmitSelectSlotAction(key string, slot shared.ScreenElementType) {
	handler.RedisClient.LPush(key, fmt.Sprintf("SELECT_SLOT %d", slot))
}

func (handler *RecipeActionServiceHandler) EmitAddIngredientAction(key string, ingredient shared.ScreenElementType, quantity int32) {
	handler.RedisClient.LPush(key, fmt.Sprintf("ADD_INGREDIENT %d %d", ingredient, quantity))
}

func (handler *RecipeActionServiceHandler) EmitAddIceAction(key string) {
	handler.RedisClient.LPush(key, "ADD_ICE")
}

func (handler *RecipeActionServiceHandler) EmitAgeAction(key string) {
	handler.RedisClient.LPush(key, "AGE")
}

func (handler *RecipeActionServiceHandler) EmitMixAction(key string, blend bool) {
	durationInSeconds := 1
	if blend {
		durationInSeconds = 5
	}
	handler.RedisClient.LPush(key, fmt.Sprintf("MIX %d", durationInSeconds))
}

func (handler *RecipeActionServiceHandler) EmitServeAction(key string) {
	handler.RedisClient.LPush(key, "SERVE")
}

func (handler *RecipeActionServiceHandler) GetRecipeActions(ctx context.Context, drinkRecipe *recipe.DrinkRecipe, reset bool, slot shared.ScreenElementType, serve bool) (int32, error) {
	fmt.Println(drinkRecipe)

	transactionId := handler.GetNextTransactionId()
	key := fmt.Sprintf("actions:%d", transactionId)

	if reset {
		handler.EmitResetAction(key)
	}

	handler.EmitSelectSlotAction(key, slot)

	for ingredient, quantity := range drinkRecipe.Quantity {
		handler.EmitAddIngredientAction(key, ingredient, quantity)
	}

	if drinkRecipe.AddIce {
		handler.EmitAddIceAction(key)
	}

	if drinkRecipe.Age {
		handler.EmitAgeAction(key)
	}

	handler.EmitMixAction(key, drinkRecipe.Blend)

	if serve {
		handler.EmitServeAction(key)
	}

	return transactionId, nil
}
