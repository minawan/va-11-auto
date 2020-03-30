package main

import (
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
	"strings"
)

type DrinkRecipeServiceHandler struct {
	RedisClient  *redis.Client
	requestQueue <-chan *redis.Message
}

type drinkRecipeSpec struct {
	transactionId int64
	drinkName     recipe.DrinkName
	addKarmotrine bool
	bigSize       bool
	reset         bool
	slot          shared.ScreenElementType
	serve         bool
	useShortcut   bool
}

func NewDrinkRecipeServiceHandler(redisClient *redis.Client) *DrinkRecipeServiceHandler {
	handler := DrinkRecipeServiceHandler{
		RedisClient:  redisClient,
		requestQueue: redisClient.Subscribe("request.queue").Channel(),
	}
	go handler.run()
	return &handler
}

func (handler *DrinkRecipeServiceHandler) find(name recipe.DrinkName) (*DrinkRecipe, error) {
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

func convertInt32ToString(value int32) string {
	return strconv.FormatInt(int64(value), 10)
}

func (handler *DrinkRecipeServiceHandler) emitRecipeInfo(key string, recipeInfo *RecipeInfo) {
	handler.RedisClient.Del(key)
	handler.RedisClient.HSet(
		key,
		"adelhyde", convertInt32ToString(recipeInfo.Adelhyde),
		"bronson_extract", convertInt32ToString(recipeInfo.BronsonExtract),
		"powdered_delta", convertInt32ToString(recipeInfo.PowderedDelta),
		"flanergide", convertInt32ToString(recipeInfo.Flanergide),
		"karmotrine", convertInt32ToString(recipeInfo.Karmotrine),
		"add_ice", strconv.FormatBool(recipeInfo.AddIce),
		"age", strconv.FormatBool(recipeInfo.Age),
		"wait", strconv.FormatBool(recipeInfo.Wait))
}

func (handler *DrinkRecipeServiceHandler) receiveDrinkRecipeRequest() (*drinkRecipeSpec, error) {
	msg := <-handler.requestQueue
	fmt.Println(msg.Channel, msg.Payload)
	numTokens := 8
	tokens := strings.SplitN(msg.Payload, " ", numTokens)
	if len(tokens) < numTokens {
		return nil, fmt.Errorf("Invalid number of tokens for a message in %s: %s - Expected: %d, Received: %d", msg.Channel, msg.Payload, numTokens, len(tokens))
	}
	transactionId, err := strconv.ParseInt(tokens[0], 10, 64)
	if err != nil {
		return nil, err
	}
	drinkName, err := strconv.ParseInt(tokens[1], 10, 64)
	if err != nil {
		return nil, err
	}
	addKarmotrine, err := strconv.ParseBool(tokens[2])
	if err != nil {
		return nil, err
	}
	bigSize, err := strconv.ParseBool(tokens[3])
	if err != nil {
		return nil, err
	}
	reset, err := strconv.ParseBool(tokens[4])
	if err != nil {
		return nil, err
	}
	slot, err := strconv.ParseInt(tokens[5], 10, 64)
	if err != nil {
		return nil, err
	}
	serve, err := strconv.ParseBool(tokens[6])
	if err != nil {
		return nil, err
	}
	useShortcut, err := strconv.ParseBool(tokens[7])
	if err != nil {
		return nil, err
	}
	return &drinkRecipeSpec{
		transactionId: transactionId,
		drinkName:     recipe.DrinkName(drinkName),
		addKarmotrine: addKarmotrine,
		bigSize:       bigSize,
		reset:         reset,
		slot:          shared.ScreenElementType(slot),
		serve:         serve,
		useShortcut:   useShortcut,
	}, nil
}

func (handler *DrinkRecipeServiceHandler) getDrinkRecipe() (int64, error) {
	spec, err := handler.receiveDrinkRecipeRequest()
	if err != nil {
		fmt.Println(err)
		return 0, err
	}

	transactionId := spec.transactionId
	drinkName := spec.drinkName
	addKarmotrine := spec.addKarmotrine
	bigSize := spec.bigSize
	reset := spec.reset
	slot := spec.slot
	serve := spec.serve
	useShortcut := spec.useShortcut

	fmt.Println(drinkName.String())
	key := fmt.Sprintf("recipe:%d", transactionId)

	if bigSize && drinkName == recipe.DrinkName_CREVICE_SPIKE {
		addKarmotrine = true
	}

	drinkRecipe, err := handler.find(drinkName)
	if err != nil {
		return 0, err
	}

	recipeInfo := drinkRecipe.Recipe

	if bigSize && !drinkRecipe.IsBig() {
		recipeInfo.Adelhyde *= 2
		recipeInfo.BronsonExtract *= 2
		recipeInfo.PowderedDelta *= 2
		recipeInfo.Flanergide *= 2
		recipeInfo.Karmotrine *= 2
	}

	if recipeInfo.Karmotrine < 0 && addKarmotrine {
		recipeInfo.Karmotrine = 1
	}

	handler.emitRecipeInfo(key, &recipeInfo)

	err = handler.RedisClient.Publish("recipe.queue", fmt.Sprintf("%d %s %d %s %s", transactionId, strconv.FormatBool(reset), slot, strconv.FormatBool(serve), strconv.FormatBool(useShortcut))).Err()
	if err != nil {
		return 0, err
	}

	return transactionId, nil
}

func (handler *DrinkRecipeServiceHandler) run() {
	for {
		id, err := handler.getDrinkRecipe()
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("DrinkRecipeService processed transaction", id)
		}
	}
}
