import sys
import os

sys.path.append('thrift/gen-py')

from action.ttypes import AddIceAction
from action.ttypes import AddIngredientAction
from action.ttypes import AgeAction
from action.ttypes import MixAction
from action.ttypes import RecipeAction
from action.ttypes import RecipeActionRequest
from action.ttypes import RecipeActionResponse
from action.ttypes import ResetAction
from action.ttypes import ScreenElementType
from action.ttypes import SelectSlotAction
from action.ttypes import ServeAction
from command.ttypes import ClickCommand
from command.ttypes import Command
from command.ttypes import CommandRequest
from command.ttypes import CommandResponse
from command.ttypes import Coord
from command.ttypes import DragAndDropCommand
from command.ttypes import TypeCommand
from command.ttypes import WaitCommand
from recipe.ttypes import DrinkName
from recipe.ttypes import DrinkRecipe
from recipe.ttypes import DrinkRecipeRequest
from recipe.ttypes import DrinkRecipeResponse

from centroid import *
from drink import *

class ScreenElement:
    elements = [ADD_ICE, AGE, LEFT_SLOT, RIGHT_SLOT, RESET, MIX, BLENDER]
    def __init__(self, entry):
        x = entry[X_COORD]
        y = entry[Y_COORD]
        self.centroid = (x, y)
        try:
            self.shortcut = str(chr(entry[SHORTCUT]))
        except KeyError:
            print('Shortcut not available for {name} of type {category}'.format(name=name, category=entry[CATEGORY]))
            self.shortcut = 'l'

    def getCentroid(self):
        return self.centroid

    def getShortcut(self):
        return self.shortcut

class Recipe:
    ingredients = [ADELHYDE, BRONSON_EXTRACT, POWDERED_DELTA, FLANERGIDE, KARMOTRINE]
    def __init__(self, recipe):
        self.recipe = recipe

    def _isBig(self):
        total_amount = 0
        for name in Recipe.ingredients:
            individual_amount = self.recipe[name]
            if individual_amount > 0:
                total_amount += individual_amount
        return total_amount > 10

    def addOpt(self):
        for name in Recipe.ingredients:
            if self.recipe[name] < 0:
                self.recipe[name] = 1

    def doubleSize(self):
        if not self._isBig():
            for name in Recipe.ingredients:
                self.recipe[name] *= 2

    def _getIngredientCount(self, ingredient_name):
        return self.recipe[ingredient_name]

    def getAdelhydeCount(self):
        return self._getIngredientCount(ADELHYDE)

    def getBronsonExtractCount(self):
        return self._getIngredientCount(BRONSON_EXTRACT)

    def getPowderedDeltaCount(self):
        return self._getIngredientCount(POWDERED_DELTA)

    def getFlanergideCount(self):
        return self._getIngredientCount(FLANERGIDE)

    def getKarmotrineCount(self):
        return self._getIngredientCount(KARMOTRINE)

    def isOnTheRocks(self):
        return self.recipe[ADD_ICE]

    def isAged(self):
        return self.recipe[AGE]

    def isBlended(self):
        return self.recipe[WAIT]


def toCoord(centroid):
    return Coord(x=centroid[0], y=centroid[1])

def trigger(screen_element, use_shortcut):
    shortcut = screen_element.getShortcut()
    if use_shortcut and shortcut != '\x00':
        return Command(typeCommand=TypeCommand(key=ord(shortcut)))
    return Command(clickCommand=ClickCommand(position=toCoord(screen_element.getCentroid())))

def getRecipeActions(request):
    actions = []
    if request.reset:
        actions.append(RecipeAction(resetAction=ResetAction()))
    actions.append(RecipeAction(selectSlotAction=SelectSlotAction(slot=request.slot)))
    drink_recipe = request.drinkRecipe
    actions.append(RecipeAction(addIngredientAction=AddIngredientAction(ingredient=ScreenElementType.ADELHYDE, amount=drink_recipe.adelhyde)))
    actions.append(RecipeAction(addIngredientAction=AddIngredientAction(ingredient=ScreenElementType.BRONSON_EXTRACT, amount=drink_recipe.bronsonExtract)))
    actions.append(RecipeAction(addIngredientAction=AddIngredientAction(ingredient=ScreenElementType.POWDERED_DELTA, amount=drink_recipe.powderedDelta)))
    actions.append(RecipeAction(addIngredientAction=AddIngredientAction(ingredient=ScreenElementType.FLANERGIDE, amount=drink_recipe.flanergide)))
    actions.append(RecipeAction(addIngredientAction=AddIngredientAction(ingredient=ScreenElementType.KARMOTRINE, amount=drink_recipe.karmotrine)))
    if drink_recipe.addIce:
        actions.append(RecipeAction(addIceAction=AddIceAction()))
    if drink_recipe.age:
        actions.append(RecipeAction(ageAction=AgeAction()))
    durationInSeconds = 5 if drink_recipe.blend else 1
    actions.append(RecipeAction(mixAction=MixAction(durationInSeconds=durationInSeconds)))
    if request.serve:
        actions.append(RecipeAction(serveAction=ServeAction()))

    return RecipeActionResponse(actions=actions)

def getActionsFromDrinkRecipe(drink_recipe, slot, serve, reset):
    return response.actions

def getCommandsFromAction(use_shortcut, action):
    screen_elements = dict()
    for name in Recipe.ingredients:
        screen_elements[name] = ScreenElement(centroid[name])
    for name in ScreenElement.elements:
        screen_elements[name] = ScreenElement(centroid[name])

    commands = []
    if action.resetAction:
        commands.append(trigger(screen_elements[RESET], use_shortcut))
    elif action.selectSlotAction:
        commands.append(trigger(screen_elements[ScreenElementType._VALUES_TO_NAMES[action.selectSlotAction.slot]], use_shortcut))
    elif action.addIngredientAction:
        source = screen_elements[ScreenElementType._VALUES_TO_NAMES[action.addIngredientAction.ingredient]]
        destination = screen_elements[BLENDER]
        for _ in range(action.addIngredientAction.amount):
            shortcut = source.getShortcut()
            if use_shortcut and shortcut != '\x00':
                commands.append(Command(typeCommand=TypeCommand(key=ord(shortcut))))
            else:
                commands.append(Command(dragAndDropCommand=DragAndDropCommand(source=toCoord(source.getCentroid()), destination=toCoord(destination.getCentroid()))))
    elif action.addIceAction:
        commands.append(trigger(screen_elements[ADD_ICE], use_shortcut))
    elif action.ageAction:
        commands.append(trigger(screen_elements[AGE], use_shortcut))
    elif action.mixAction:
        commands.append(trigger(screen_elements[MIX], use_shortcut))
        commands.append(Command(waitCommand=WaitCommand(durationInSeconds=action.mixAction.durationInSeconds)))
        commands.append(trigger(screen_elements[MIX], use_shortcut))
    elif action.serveAction:
        commands.append(trigger(screen_elements[MIX], use_shortcut))
    else:
        print('Unexpected recipe action type:', action.__class__.__name__)

    return CommandResponse(commands=commands)

def execute(command):
    if command.clickCommand:
        position = command.clickCommand.position
        print('mousemove {x} {y} '.format(x=position.x, y=position.y), end='')
        print('sleep 0.5 ', end='')
        print('mousedown 1 ', end='')
        print('sleep 0.5 ', end='')
        print('mouseup 1 ', end='')
        print('sleep 0.5 ', end='')
    elif command.dragAndDropCommand:
        source = command.dragAndDropCommand.source
        destination = command.dragAndDropCommand.destination
        print('mousemove {x} {y} '.format(x=source.x, y=source.y), end='')
        print('sleep 0.5 ', end='')
        print('mousedown 1 ', end='')
        print('mousemove {x} {y} '.format(x=destination.x, y=destination.y), end='')
        print('sleep 0.5 ', end='')
        print('mouseup 1 ', end='')
    elif command.waitCommand:
        print('sleep {seconds} '.format(seconds=command.waitCommand.durationInSeconds), end='')
    elif command.typeCommand:
        print('key {shortcut} '.format(shortcut=chr(command.typeCommand.key), end=''))
    else:
        print('Unexpected command type:', str(command))

def getDrinkRecipe(request):
    drink_recipe = Recipe(drink[DrinkName._VALUES_TO_NAMES[request.drinkName]][RECIPE])

    if request.bigSize and request.drinkName == DrinkName.CREVICE_SPIKE:
        request.addKarmotrine = True

    if request.bigSize:
        drink_recipe.doubleSize()

    if request.addKarmotrine:
        drink_recipe.addOpt()

    return DrinkRecipeResponse(
               drinkRecipe=DrinkRecipe(
                   adelhyde=drink_recipe.getAdelhydeCount(),
                   bronsonExtract=drink_recipe.getBronsonExtractCount(),
                   flanergide=drink_recipe.getFlanergideCount(),
                   powderedDelta=drink_recipe.getPowderedDeltaCount(),
                   karmotrine=drink_recipe.getKarmotrineCount(),
                   addIce=drink_recipe.isOnTheRocks(),
                   age=drink_recipe.isAged(),
                   blend=drink_recipe.isBlended()))

def getCommands(command_request):
    drink_recipe_request = DrinkRecipeRequest(
                               drinkName=command_request.drinkName,
                               addKarmotrine=command_request.addKarmotrine,
                               bigSize=command_request.bigSize)

    drink_recipe_response = getDrinkRecipe(drink_recipe_request)

    recipe_action_request = RecipeActionRequest(
                                drinkRecipe=drink_recipe_response.drinkRecipe,
                                reset=command_request.reset,
                                slot=command_request.slot,
                                serve=command_request.serve)
    recipe_action_response = getRecipeActions(recipe_action_request)
    commands = []
    for action in recipe_action_response.actions:
        commands.extend(getCommandsFromAction(
                            command_request.useShortcut,
                            action).commands)
    return CommandResponse(commands=commands)

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

    command_request = CommandRequest(
                          drinkName=drink_name,
                          addKarmotrine=add_opt,
                          bigSize=double,
                          reset=reset,
                          slot=slot,
                          serve=serve,
                          useShortcut=use_shortcut)

    print('xdotool search --name "VA-11 Hall-A: Cyberpunk Bartender Action" ', end='')
    for command in getCommands(command_request).commands:
        execute(command)
    print()

if __name__ == '__main__':
    main()
