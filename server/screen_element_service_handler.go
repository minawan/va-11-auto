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

func (handler *ScreenElementServiceHandler) GetScreenElement(ctx context.Context, request *element.ScreenElementRequest) (*element.ScreenElementResponse, error) {
	fmt.Println(request)

	var response element.ScreenElementResponse
	for _, screenElement := range *handler.ScreenElements {
		if screenElement.Name != request.ScreenElementName.String() {
			continue
		}
		centroid := shared.NewCoord()
		centroid.X = screenElement.XCoord
		centroid.Y = screenElement.YCoord
		response.ScreenElement = shared.NewScreenElement()
		response.ScreenElement.Centroid = centroid
		response.ScreenElement.Shortcut = screenElement.Shortcut
		fmt.Println(response)
		return &response, nil
	}

	return nil, errors.New(fmt.Sprintf("ScreenElement for %s not found!", request.ScreenElementName.String()))
}
