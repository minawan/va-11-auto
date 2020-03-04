//+build wireinject

package main

import (
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/google/wire"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
)

func CreateMultiplexedProcessor(recipeActionServiceHandler *RecipeActionServiceHandler, drinkRecipeServiceHandler *DrinkRecipeServiceHandler) thrift.TProcessor {
	processor := thrift.NewTMultiplexedProcessor()
	processor.RegisterProcessor("RecipeActionService", action.NewRecipeActionServiceProcessor(recipeActionServiceHandler))
	processor.RegisterProcessor("DrinkRecipeService", recipe.NewDrinkRecipeServiceProcessor(drinkRecipeServiceHandler))
	return processor
}

func CreateCommandServer(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory, serverSocket thrift.TServerTransport, recipes *[]DrinkRecipe) (*thrift.TSimpleServer, error) {
	wire.Build(thrift.NewTSimpleServer4, CreateMultiplexedProcessor, NewRecipeActionServiceHandler, NewDrinkRecipeServiceHandler)
	return nil, nil
}
