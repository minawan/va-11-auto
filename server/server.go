package main

import (
	"fmt"
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/minawan/va-11-auto/thrift/gen-go/action"
)

func runServer(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory, addr string) error {
	transport, err := thrift.NewTServerSocket(addr)

	if err != nil {
		return err
	}

	processor := thrift.NewTMultiplexedProcessor()
	processor.RegisterProcessor("RecipeActionServer", action.NewRecipeActionServerProcessor(NewRecipeActionServerHandler()))
	server := thrift.NewTSimpleServer4(processor, transport, transportFactory, protocolFactory)

	fmt.Println("Running RecipeActionServer on ", addr)
	return server.Serve()
}
