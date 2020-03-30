package main

import (
	"context"
	"flag"
	"fmt"
	"github.com/apache/thrift/lib/go/thrift"
	"github.com/minawan/va-11-auto/thrift/gen-go/command"
	"github.com/minawan/va-11-auto/thrift/gen-go/recipe"
	"github.com/minawan/va-11-auto/thrift/gen-go/shared"
	"log"
	"os"
)

const bufferSize = 8192

func usage() {
	fmt.Fprint(os.Stderr, "Usage of ", os.Args[0], ":\n")
	flag.PrintDefaults()
	fmt.Fprint(os.Stderr, "\n")
}

func main() {
	flag.Usage = usage
	addr := flag.String("addr", "localhost:9090", "Address to listen to")
	flag.Parse()

	//addKarmotrine := true
	addKarmotrine := false
	//serve := true
	serve := false
	slot := shared.ScreenElementType_LEFT_SLOT
	//slot := shared.ScreenElementType_RIGHT_SLOT
	//bigSize := true
	bigSize := false
	useShortcut := true
	//useShortcut := false
	reset := true
	//reset := false

	//drinkName := recipe.DrinkName_BAD_TOUCH
	drinkName := recipe.DrinkName_BEER
	//drinkName := recipe.DrinkName_BLEEDING_JANE
	//drinkName := recipe.DrinkName_BLOOM_LIGHT
	//drinkName := recipe.DrinkName_BLUE_FAIRY
	//drinkName := recipe.DrinkName_BRANDTINI
	//drinkName := recipe.DrinkName_COBALT_VELVET
	//drinkName := recipe.DrinkName_CREVICE_SPIKE
	//drinkName := recipe.DrinkName_FLUFFY_DREAM
	//drinkName := recipe.DrinkName_FRINGE_WEAVER
	//drinkName := recipe.DrinkName_FROTHY_WATER
	//drinkName := recipe.DrinkName_GRIZZLY_TEMPLE
	//drinkName := recipe.DrinkName_GUT_PUNCH
	//drinkName := recipe.DrinkName_MARSBLAST
	//drinkName := recipe.DrinkName_MERCURYBLAST
	//drinkName := recipe.DrinkName_MOONBLAST
	//drinkName := recipe.DrinkName_PIANO_MAN
	//drinkName := recipe.DrinkName_PIANO_WOMAN
	//drinkName := recipe.DrinkName_PILEDRIVER
	//drinkName := recipe.DrinkName_SPARKLE_STAR
	//drinkName := recipe.DrinkName_SUGAR_RUSH
	//drinkName := recipe.DrinkName_SUNSHINE_CLOUD
	//drinkName := recipe.DrinkName_SUPLEX
	//drinkName := recipe.DrinkName_ZEN_STAR
	//drinkName := recipe.DrinkName_FLAMING_MOAI

	transportFactory := thrift.NewTBufferedTransportFactory(bufferSize)
	//protocolFactory := thrift.NewTBinaryProtocolFactoryDefault()
	socket, err := thrift.NewTSocket(*addr)
	if err != nil {
		panic(fmt.Sprintf("Error opening socket: %s", err))
	}
	transport, err := transportFactory.GetTransport(socket)
	if err != nil {
		panic(fmt.Sprintf("Error creating transport: %s", err))
	}
	defer transport.Close()
	if err := transport.Open(); err != nil {
		panic(fmt.Sprintf("Error opening transport: %s", err))
	}
	protocol := thrift.NewTBinaryProtocolTransport(transport)
	commandProtocol := thrift.NewTMultiplexedProtocol(protocol, "CommandService")
	defaultContext := context.Background()
	client := command.NewCommandServiceClientProtocol(transport, commandProtocol, commandProtocol)
	commands, err := client.GetCommands(defaultContext, drinkName, addKarmotrine, bigSize, reset, slot, serve, useShortcut)
	if err != nil {
		panic(err)
	}
	for _, cmd := range commands {
		if cmd.ClickCommand != nil {
			position := cmd.ClickCommand.Position
			fmt.Println("mousemove", position.X, position.Y)
			fmt.Println("sleep 0.5")
			fmt.Println("mousedown 1")
			fmt.Println("sleep 0.5")
			fmt.Println("mouseup 1")
			fmt.Println("sleep 0.5")
		} else if cmd.DragAndDropCommand != nil {
			source := cmd.DragAndDropCommand.Source
			destination := cmd.DragAndDropCommand.Destination
			fmt.Println("mousemove", source.X, source.Y)
			fmt.Println("sleep 0.5")
			fmt.Println("mousedown 1")
			fmt.Println("mousemove", destination.X, destination.Y)
			fmt.Println("sleep 0.5")
			fmt.Println("mouseup 1")
		} else if cmd.WaitCommand != nil {
			fmt.Println("sleep", cmd.WaitCommand.DurationInSeconds)
		} else if cmd.TypeCommand != nil {
			shortcut := string(rune(cmd.TypeCommand.Key))
			if shortcut == " " {
				shortcut = "space"
			}
			fmt.Println("key --delay 100", shortcut)
		} else {
			log.Println("Unrecognized type for command:", cmd)
		}
	}
}
