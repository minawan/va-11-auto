package main

import (
	"fmt"
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
)

func runServer(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory, addr string, recipes *[]DrinkRecipe) error {
	transport, err := thrift.NewTServerSocket(addr)

	if err != nil {
		return err
	}

	processor := thrift.NewTMultiplexedProcessor()
	processor.RegisterProcessor("RecipeActionService", action.NewRecipeActionServiceProcessor(NewRecipeActionServiceHandler()))
	processor.RegisterProcessor("DrinkRecipeService", recipe.NewDrinkRecipeServiceProcessor(NewDrinkRecipeServiceHandler(recipes)))
	server := thrift.NewTSimpleServer4(processor, transport, transportFactory, protocolFactory)

	fmt.Println("Running RecipeActionService on ", addr)
	return server.Serve()
}
