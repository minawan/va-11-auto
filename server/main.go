package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/apache/thrift/lib/go/thrift"
	"io/ioutil"
	"os"
)

const bufferSize = 8192

func Usage() {
	fmt.Fprint(os.Stderr, "Usage of ", os.Args[0], ":\n")
	flag.PrintDefaults()
	fmt.Fprint(os.Stderr, "\n")
}

func main() {
	flag.Usage = Usage
	addr := flag.String("addr", "localhost:9090", "Address to listen to")
	recipePath := flag.String("recipe-path", "./DrinkRecipe.json", "Path to recipe json")
	flag.Parse()

	fmt.Println("Reading drink recipe from", *recipePath)
	content, err := ioutil.ReadFile(*recipePath)
	if err != nil {
		panic(err)
	}
	var recipes []DrinkRecipe
	json.Unmarshal(content, &recipes)

	protocolFactory := thrift.NewTBinaryProtocolFactoryDefault()
	transportFactory := thrift.NewTBufferedTransportFactory(bufferSize)

	serverSocket, err := thrift.NewTServerSocket(*addr)
	if err != nil {
		fmt.Println("error creating server socket:", err)
		return
	}

	fmt.Println("Running RecipeActionService on", *addr)
	commandServer, err := CreateCommandServer(transportFactory, protocolFactory, serverSocket, &recipes)
	if err != nil {
		fmt.Println("error creating server:", err)
	} else if err := commandServer.Serve(); err != nil {
		fmt.Println("error running server:", err)
	}
}
