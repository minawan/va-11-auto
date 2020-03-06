import sys
import os

sys.path.append('thrift/gen-py')

from action import RecipeActionService
from action.ttypes import RecipeActionRequest
from command import CommandService
from command.ttypes import ClickCommand
from command.ttypes import Command
from command.ttypes import CommandRequest
from command.ttypes import CommandResponse
from command.ttypes import DragAndDropCommand
from command.ttypes import TypeCommand
from command.ttypes import WaitCommand
from element import ScreenElementService
from element.ttypes import ScreenElementRequest
from recipe import DrinkRecipeService
from recipe.ttypes import DrinkName
from shared.ttypes import Coord
from shared.ttypes import ScreenElementType
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.protocol import TMultiplexedProtocol


def trigger(screen_element, use_shortcut):
    shortcut = screen_element.shortcut
    if use_shortcut and shortcut != '\x00':
        return Command(typeCommand=TypeCommand(key=ord(shortcut)))
    return Command(clickCommand=ClickCommand(position=screen_element.centroid))

def execute(command):
    if command.clickCommand:
        position = command.clickCommand.position
        print('    mousemove {x} {y} \\'.format(x=position.x, y=position.y))
        print('    sleep 0.5 \\')
        print('    mousedown 1 \\')
        print('    sleep 0.5 \\')
        print('    mouseup 1 \\')
        print('    sleep 0.5 \\')
    elif command.dragAndDropCommand:
        source = command.dragAndDropCommand.source
        destination = command.dragAndDropCommand.destination
        print('    mousemove {x} {y} \\'.format(x=source.x, y=source.y))
        print('    sleep 0.5 \\')
        print('    mousedown 1 \\')
        print('    mousemove {x} {y} \\'.format(x=destination.x, y=destination.y))
        print('    sleep 0.5 \\')
        print('    mouseup 1 \\')
    elif command.waitCommand:
        print('    sleep {seconds} \\'.format(seconds=command.waitCommand.durationInSeconds))
    elif command.typeCommand:
        print('    key {shortcut} \\'.format(shortcut=chr(command.typeCommand.key)))
    else:
        sys.stderr.write('Unexpected command type: {}\n'.format(str(command)))

def getCommands(command_request):
    socket = TSocket.TSocket('localhost', 9090)
    transport = TTransport.TBufferedTransport(socket)

    protocol = TBinaryProtocol.TBinaryProtocol(transport)
    drink_recipe_protocol = TMultiplexedProtocol.TMultiplexedProtocol(protocol, 'DrinkRecipeService')
    recipe_action_protocol = TMultiplexedProtocol.TMultiplexedProtocol(protocol, 'RecipeActionService')
    screen_element_protocol = TMultiplexedProtocol.TMultiplexedProtocol(protocol, 'ScreenElementService')
    command_protocol = TMultiplexedProtocol.TMultiplexedProtocol(protocol, 'CommandService')

    drink_recipe_client = DrinkRecipeService.Client(drink_recipe_protocol)
    recipe_action_client = RecipeActionService.Client(recipe_action_protocol)
    screen_element_client = ScreenElementService.Client(screen_element_protocol)
    command_client = CommandService.Client(command_protocol)

    transport.open()

    drink_recipe = drink_recipe_client.getDrinkRecipe(
                       drinkName=command_request.drinkName,
                       addKarmotrine=command_request.addKarmotrine,
                       bigSize=command_request.bigSize)

    recipe_action_request = RecipeActionRequest(
                                drinkRecipe=drink_recipe,
                                reset=command_request.reset,
                                slot=command_request.slot,
                                serve=command_request.serve)

    recipe_action_response = recipe_action_client.getRecipeActions(recipe_action_request)

    screen_elements = dict()
    for name in [name for name in ScreenElementType._VALUES_TO_NAMES]:
        screen_element_request = ScreenElementRequest(screenElementName=name)
        screen_element_response = screen_element_client.getScreenElement(screen_element_request)
        screen_elements[name] = screen_element_response.screenElement


    commands = []
    for action in recipe_action_response.actions:
        command_response = command_client.convertActionToCommands(screen_elements, action, command_request.useShortcut)
        commands.extend(command_response.commands)

    transport.close()

    return CommandResponse(commands=commands)

def main():
    add_opt = True
    #add_opt = False
    serve = True
    #serve = False
    slot = ScreenElementType.LEFT_SLOT
    #slot = ScreenElementType.RIGHT_SLOT
    #double = True
    double = False
    #use_shortcut = True
    use_shortcut = False
    reset = True
    #reset = False

    #drink_name = DrinkName.BAD_TOUCH
    #drink_name = DrinkName.BEER
    #drink_name = DrinkName.BLEEDING_JANE
    #drink_name = DrinkName.BLOOM_LIGHT
    #drink_name = DrinkName.BLUE_FAIRY
    #drink_name = DrinkName.BRANDTINI
    #drink_name = DrinkName.COBALT_VELVET
    #drink_name = DrinkName.CREVICE_SPIKE
    #drink_name = DrinkName.FLUFFY_DREAM
    drink_name = DrinkName.FRINGE_WEAVER
    #drink_name = DrinkName.FROTHY_WATER
    #drink_name = DrinkName.GRIZZLY_TEMPLE
    #drink_name = DrinkName.GUT_PUNCH
    #drink_name = DrinkName.MARSBLAST
    #drink_name = DrinkName.MERCURYBLAST
    #drink_name = DrinkName.MOONBLAST
    #drink_name = DrinkName.PIANO_MAN
    #drink_name = DrinkName.PIANO_WOMAN
    #drink_name = DrinkName.PILEDRIVER
    #drink_name = DrinkName.SPARKLE_STAR
    #drink_name = DrinkName.SUGAR_RUSH
    #drink_name = DrinkName.SUNSHINE_CLOUD
    #drink_name = DrinkName.SUPLEX
    #drink_name = DrinkName.ZEN_STAR
    #drink_name = DrinkName.FLAMING_MOAI

    command_request = CommandRequest(
                          drinkName=drink_name,
                          addKarmotrine=add_opt,
                          bigSize=double,
                          reset=reset,
                          slot=slot,
                          serve=serve,
                          useShortcut=use_shortcut)

    print('xdotool search --name "VA-11 Hall-A: Cyberpunk Bartender Action" \\')
    for command in getCommands(command_request).commands:
        execute(command)
    print()

if __name__ == '__main__':
    main()
