//+build wireinject

package main

import (
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/go-redis/redis/v7"
	"github.com/google/wire"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
)

type CommandServer struct {
	commandServer *thrift.TSimpleServer
	drinkRecipeServiceHandler *DrinkRecipeServiceHandler
	recipeActionServiceHandler *RecipeActionServiceHandler
}

func (server *CommandServer) Serve() error {
	return server.commandServer.Serve()
}

func CreateCommandServer(
	transportFactory thrift.TTransportFactory,
	protocolFactory thrift.TProtocolFactory,
	serverSocket thrift.TServerTransport,
	redisClient *redis.Client) (*CommandServer, error) {
	wire.Build(
		wire.Struct(new(CommandServer), "*"),
		thrift.NewTSimpleServer4,
		wire.Bind(new(thrift.TProcessor), new(*command.CommandServiceProcessor)),
		command.NewCommandServiceProcessor,
		NewRecipeActionServiceHandler,
		NewDrinkRecipeServiceHandler,
		NewCommandServiceHandler)
	return nil, nil
}
