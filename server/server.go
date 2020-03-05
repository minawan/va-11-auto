//+build wireinject

package main

import (
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/google/wire"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/element"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
)

func CreateMultiplexedProcessor(recipeActionServiceProcessor *action.RecipeActionServiceProcessor, drinkRecipeServiceProcessor *recipe.DrinkRecipeServiceProcessor, screenElementServiceProcessor *element.ScreenElementServiceProcessor) thrift.TProcessor {
	processor := thrift.NewTMultiplexedProcessor()
	processor.RegisterProcessor("RecipeActionService", recipeActionServiceProcessor)
	processor.RegisterProcessor("DrinkRecipeService", drinkRecipeServiceProcessor)
	processor.RegisterProcessor("ScreenElementService", screenElementServiceProcessor)
	return processor
}

func CreateCommandServer(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory, serverSocket thrift.TServerTransport, recipes *[]DrinkRecipe, screenElements *[]ScreenElement) (*thrift.TSimpleServer, error) {
	wire.Build(thrift.NewTSimpleServer4, CreateMultiplexedProcessor, action.NewRecipeActionServiceProcessor, recipe.NewDrinkRecipeServiceProcessor, element.NewScreenElementServiceProcessor, NewRecipeActionServiceHandler, NewDrinkRecipeServiceHandler, NewScreenElementServiceHandler)
	return nil, nil
}
