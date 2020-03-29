package main

import (
	"errors"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
	"strings"
)

type RecipeActionServiceHandler struct {
	RedisClient *redis.Client
	recipeQueue <-chan *redis.Message
}

type recipeActionSpec struct {
	transactionId int64
	reset         bool
	slot          shared.ScreenElementType
	serve         bool
	useShortcut   bool
}

func NewRecipeActionServiceHandler(redisClient *redis.Client) action.RecipeActionService {
	handler := RecipeActionServiceHandler{RedisClient: redisClient, recipeQueue: redisClient.Subscribe("recipe.queue").Channel()}
	go handler.run()
	return &handler
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

func getFlagFromDrinkRecipeMap(drinkRecipeMap map[string]string, flagName string) (bool, error) {
	if flagValue, ok := drinkRecipeMap[flagName]; ok {
		return strconv.ParseBool(flagValue)
	}
	return false, errors.New(fmt.Sprintf("Flag %s not found.", flagName))
}

func (handler *RecipeActionServiceHandler) loadDrinkRecipe(transactionId int64) (drinkRecipe *recipe.DrinkRecipe, err error) {
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
	drinkRecipe.AddIce, err = getFlagFromDrinkRecipeMap(drinkRecipeMap, "add_ice")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Age, err = getFlagFromDrinkRecipeMap(drinkRecipeMap, "age")
	if err != nil {
		return nil, err
	}
	drinkRecipe.Blend, err = getFlagFromDrinkRecipeMap(drinkRecipeMap, "wait")
	if err != nil {
		return nil, err
	}
	return drinkRecipe, nil
}

func (handler *RecipeActionServiceHandler) receiveDrinkRecipe() (*recipeActionSpec, error) {
	msg := <-handler.recipeQueue
	fmt.Println(msg.Channel, msg.Payload)
	numTokens := 5
	tokens := strings.SplitN(msg.Payload, " ", numTokens)
	if len(tokens) < numTokens {
		return nil, fmt.Errorf("Invalid number of tokens for a message in %s: %s - Expected: %d, Received: %d", msg.Channel, msg.Payload, numTokens, len(tokens))
	}
	transactionId, err := strconv.ParseInt(tokens[0], 10, 64)
	if err != nil {
		return nil, err
	}
	reset, err := strconv.ParseBool(tokens[1])
	if err != nil {
		return nil, err
	}
	slot, err := strconv.ParseInt(tokens[2], 10, 64)
	if err != nil {
		return nil, err
	}
	serve, err := strconv.ParseBool(tokens[3])
	if err != nil {
		return nil, err
	}
	useShortcut, err := strconv.ParseBool(tokens[4])
	if err != nil {
		return nil, err
	}
	return &recipeActionSpec{transactionId: transactionId, reset: reset, slot: shared.ScreenElementType(slot), serve: serve, useShortcut: useShortcut}, nil
}

func (handler *RecipeActionServiceHandler) getRecipeActions() (int64, error) {
	spec, err := handler.receiveDrinkRecipe()
	if err != nil {
		fmt.Println(err)
		return 0, err
	}

	transactionId := spec.transactionId
	reset := spec.reset
	slot := spec.slot
	serve := spec.serve
	useShortcut := spec.useShortcut

	key := fmt.Sprintf("actions:%d", transactionId)
	drinkRecipe, err := handler.loadDrinkRecipe(transactionId)
	if err != nil {
		fmt.Println(err)
		return 0, err
	}
	fmt.Println(drinkRecipe)

	handler.RedisClient.Del(key)
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

	err = handler.RedisClient.Publish(fmt.Sprintf("actions.queue.%d", transactionId), fmt.Sprintf("%d %s", transactionId, strconv.FormatBool(useShortcut))).Err()
	if err != nil {
		return 0, err
	}

	return transactionId, nil
}

func (handler *RecipeActionServiceHandler) run() {
	for {
		id, err := handler.getRecipeActions()
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("RecipeActionService processed transaction", id)
		}
	}
}
