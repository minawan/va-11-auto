package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type ScreenElementServiceHandler struct {
	ScreenElements *map[string]ScreenElement
}

func NewScreenElementServiceHandler(screenElements *map[string]ScreenElement) shared.ScreenElementService {
	return &ScreenElementServiceHandler{ScreenElements: screenElements}
}

func (handler *ScreenElementServiceHandler) Find(name shared.ScreenElementType) (*ScreenElement, error) {
	if screenElement, ok := (*handler.ScreenElements)[name.String()]; ok {
		return &screenElement, nil
	}
	return nil, errors.New(fmt.Sprintf("ScreenElement for %s not found!", name.String()))
}

func (handler *ScreenElementServiceHandler) GetScreenElement(ctx context.Context, screenElementName shared.ScreenElementType) (*shared.ScreenElement, error) {
	fmt.Println(screenElementName)

	screenElement, err := handler.Find(screenElementName)
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
