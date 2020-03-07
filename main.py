import sys
import os

sys.path.append('thrift/gen-py')

from action import RecipeActionService
from command import CommandService
from recipe import DrinkRecipeService
from recipe.ttypes import DrinkName
from shared import ScreenElementService
from shared.ttypes import ScreenElementType
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.protocol import TMultiplexedProtocol


def execute(command):
    if command.clickCommand:
        position = command.clickCommand.position
        print('mousemove {x} {y}'.format(x=position.x, y=position.y))
        print('sleep 0.5')
        print('mousedown 1')
        print('sleep 0.5')
        print('mouseup 1')
        print('sleep 0.5')
    elif command.dragAndDropCommand:
        source = command.dragAndDropCommand.source
        destination = command.dragAndDropCommand.destination
        print('mousemove {x} {y}'.format(x=source.x, y=source.y))
        print('sleep 0.5')
        print('mousedown 1')
        print('mousemove {x} {y}'.format(x=destination.x, y=destination.y))
        print('sleep 0.5')
        print('mouseup 1')
    elif command.waitCommand:
        print('sleep {seconds}'.format(seconds=command.waitCommand.durationInSeconds))
    elif command.typeCommand:
        print('key {shortcut}'.format(shortcut=chr(command.typeCommand.key)))
    else:
        sys.stderr.write('Unexpected command type: {}\n'.format(str(command)))

def getCommands(drink_name, add_opt, double, reset, slot, serve, use_shortcut):
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
                       drinkName=drink_name,
                       addKarmotrine=add_opt,
                       bigSize=double)

    screen_elements = {
      name: screen_element_client.getScreenElement(screenElementName=name) for name in ScreenElementType._VALUES_TO_NAMES
    }

    commands = []
    for action in recipe_action_client.getRecipeActions(drinkRecipe=drink_recipe, reset=reset, slot=slot, serve=serve):
        cmds = command_client.convertActionToCommands(screenElements=screen_elements, action=action, useShortcut=use_shortcut)
        commands.extend(cmds)

    transport.close()

    return commands

def main():
    #add_opt = True
    add_opt = False
    #serve = True
    serve = False
    slot = ScreenElementType.LEFT_SLOT
    #slot = ScreenElementType.RIGHT_SLOT
    #double = True
    double = False
    #use_shortcut = True
    use_shortcut = False
    reset = True
    #reset = False

    #drink_name = DrinkName.BAD_TOUCH
    drink_name = DrinkName.BEER
    #drink_name = DrinkName.BLEEDING_JANE
    #drink_name = DrinkName.BLOOM_LIGHT
    #drink_name = DrinkName.BLUE_FAIRY
    #drink_name = DrinkName.BRANDTINI
    #drink_name = DrinkName.COBALT_VELVET
    #drink_name = DrinkName.CREVICE_SPIKE
    #drink_name = DrinkName.FLUFFY_DREAM
    #drink_name = DrinkName.FRINGE_WEAVER
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

    print('search --name "VA-11 Hall-A: Cyberpunk Bartender Action"')
    for command in getCommands(drink_name, add_opt, double, reset, slot, serve, use_shortcut):
        execute(command)

if __name__ == '__main__':
    main()
