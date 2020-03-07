package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/minawan/va-11-auto/thrift/gen-go/element"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

type ScreenElementServiceHandler struct {
	ScreenElements *[]ScreenElement
}

func NewScreenElementServiceHandler(screenElements *[]ScreenElement) element.ScreenElementService {
	return &ScreenElementServiceHandler{ScreenElements: screenElements}
}

func (handler *ScreenElementServiceHandler) GetScreenElement(ctx context.Context, screenElementName shared.ScreenElementType) (*shared.ScreenElement, error) {
	fmt.Println(screenElementName)

	for _, screenElement := range *handler.ScreenElements {
		if screenElement.Name != screenElementName.String() {
			continue
		}
		centroid := shared.NewCoord()
		centroid.X = screenElement.XCoord
		centroid.Y = screenElement.YCoord
		screenElement := shared.NewScreenElement()
		screenElement.Centroid = centroid
		screenElement.Shortcut = screenElement.Shortcut
		fmt.Println(screenElement)
		return screenElement, nil
	}

	return nil, errors.New(fmt.Sprintf("ScreenElement for %s not found!", screenElementName.String()))
}
