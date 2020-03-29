package main

import (
	"context"
	"fmt"
	"github.com/go-redis/redis/v7"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"strconv"
)

type ScreenElementServiceHandler struct {
	RedisClient *redis.Client
}

func NewScreenElementServiceHandler(redisClient *redis.Client) shared.ScreenElementService {
	return &ScreenElementServiceHandler{RedisClient: redisClient}
}

func (handler *ScreenElementServiceHandler) find(name shared.ScreenElementType) (*ScreenElement, error) {
	var screenElement ScreenElement
	screenElementMap, err := handler.RedisClient.HGetAll("element:" + name.String()).Result()
	if err != nil {
		return nil, err
	}

	if xCoordValue, ok := screenElementMap["x_coord"]; ok {
		xCoord, err := strconv.ParseInt(xCoordValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.XCoord = int32(xCoord)
	}
	if yCoordValue, ok := screenElementMap["y_coord"]; ok {
		yCoord, err := strconv.ParseInt(yCoordValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.YCoord = int32(yCoord)
	}
	if shortcutValue, ok := screenElementMap["shortcut"]; ok {
		shortcut, err := strconv.ParseInt(shortcutValue, 10, 32)
		if err != nil {
			return nil, err
		}
		screenElement.Shortcut = int32(shortcut)
	}

	return &screenElement, nil
}

func (handler *ScreenElementServiceHandler) GetScreenElement(ctx context.Context, screenElementName shared.ScreenElementType) (*shared.ScreenElement, error) {
	fmt.Println(screenElementName)

	screenElement, err := handler.find(screenElementName)
	if err != nil {
		return nil, err
	}
	centroid := shared.NewCoord()
	centroid.X = screenElement.XCoord
	centroid.Y = screenElement.YCoord
	response := shared.NewScreenElement()
	response.Centroid = centroid
	response.Shortcut = screenElement.Shortcut

	fmt.Println(response)
	return response, nil
}
