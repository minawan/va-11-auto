//+build wireinject

package main

import (
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/go-redis/redis/v7"
	"github.com/google/wire"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
)

func createMultiplexedProcessor(
	recipeActionServiceProcessor *action.RecipeActionServiceProcessor,
	drinkRecipeServiceProcessor *recipe.DrinkRecipeServiceProcessor,
	screenElementServiceProcessor *shared.ScreenElementServiceProcessor,
	commandServiceProcessor *command.CommandServiceProcessor) thrift.TProcessor {
	processor := thrift.NewTMultiplexedProcessor()
	processor.RegisterProcessor("RecipeActionService", recipeActionServiceProcessor)
	processor.RegisterProcessor("DrinkRecipeService", drinkRecipeServiceProcessor)
	processor.RegisterProcessor("ScreenElementService", screenElementServiceProcessor)
	processor.RegisterProcessor("CommandService", commandServiceProcessor)
	return processor
}

func CreateCommandServer(
	transportFactory thrift.TTransportFactory,
	protocolFactory thrift.TProtocolFactory,
	serverSocket thrift.TServerTransport,
	redisClient *redis.Client) (*thrift.TSimpleServer, error) {
	wire.Build(
		thrift.NewTSimpleServer4,
		createMultiplexedProcessor,
		action.NewRecipeActionServiceProcessor,
		recipe.NewDrinkRecipeServiceProcessor,
		shared.NewScreenElementServiceProcessor,
		command.NewCommandServiceProcessor,
		NewRecipeActionServiceHandler,
		NewDrinkRecipeServiceHandler,
		NewScreenElementServiceHandler,
		NewCommandServiceHandler)
	return nil, nil
}
