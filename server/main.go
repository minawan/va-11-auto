package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/go-redis/redis/v7"
	"io/ioutil"
	"os"
	"strconv"
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
	recipePath := flag.String("recipe-path", "../output/DrinkRecipe.json", "Path to recipe json")
	screenElementPath := flag.String("screen-element-path", "../output/ScreenElement.json", "Path to screen element json")
	flag.Parse()

	redisClient := redis.NewClient(&redis.Options{
			Addr: "localhost:6379",
			Password: "",
			DB: 0,
	})

	pong, err := redisClient.Ping().Result()
	if err != nil {
		panic(err)
	}
	fmt.Println(pong)

	fmt.Println("Reading drink recipe from", *recipePath)
	recipeContent, err := ioutil.ReadFile(*recipePath)
	if err != nil {
		panic(err)
	}

	var recipes map[string]DrinkRecipe
	json.Unmarshal(recipeContent, &recipes)

	for k, v := range recipes {
		redisClient.HSet("recipe:" + k, "adelhyde", strconv.FormatInt(int64(v.Recipe.Adelhyde), 10), "bronson_extract", strconv.FormatInt(int64(v.Recipe.BronsonExtract), 10), "powdered_delta", strconv.FormatInt(int64(v.Recipe.PowderedDelta), 10), "flanergide", strconv.FormatInt(int64(v.Recipe.Flanergide), 10), "karmotrine", strconv.FormatInt(int64(v.Recipe.Karmotrine), 10), "add_ice", strconv.FormatBool(v.Recipe.AddIce), "age", strconv.FormatBool(v.Recipe.Age), "wait", strconv.FormatBool(v.Recipe.Wait))
	}

	fmt.Println("Reading screen element from", *screenElementPath)
	screenElementContent, err := ioutil.ReadFile(*screenElementPath)
	if err != nil {
		panic(err)
	}
	var screenElements map[string]ScreenElement
	json.Unmarshal(screenElementContent, &screenElements)

	protocolFactory := thrift.NewTBinaryProtocolFactoryDefault()
	transportFactory := thrift.NewTBufferedTransportFactory(bufferSize)

	serverSocket, err := thrift.NewTServerSocket(*addr)
	if err != nil {
		fmt.Println("error creating server socket:", err)
		return
	}

	fmt.Println("Running command server on", *addr)
	commandServer, err := CreateCommandServer(transportFactory, protocolFactory, serverSocket, redisClient, &screenElements)
	if err != nil {
		fmt.Println("error creating server:", err)
	} else if err := commandServer.Serve(); err != nil {
		fmt.Println("error running server:", err)
	}
}
