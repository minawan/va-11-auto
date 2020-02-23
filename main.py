import sys
import os

sys.path.append('thrift/gen-py')

from action.ttypes import ScreenElementType
from command.ttypes import CommandRequest
from recipe.ttypes import DrinkName
from recipe.ttypes import DrinkRecipe
from recipe.ttypes import DrinkRecipeRequest
from recipe.ttypes import DrinkRecipeResponse

from centroid import *
from drink import *

class ClickCommand:
    def __init__(self, source):
        self.source = source

class DragAndDropCommand:
    def __init__(self, source, destination):
        self.source = source
        self.destination = destination

class WaitCommand:
    def __init__(self, seconds):
        self.seconds = seconds

class TypeCommand:
    def __init__(self, shortcut):
        self.shortcut = shortcut

class RecipeAction(object):
    def __init__(self, source, destination):
        self.source = source
        self.destination = destination
    def getSource(self):
        return self.source
    def getDestination(self):
        return self.destination

class SingleElementRecipeAction(RecipeAction):
    def __init__(self, source):
        super(SingleElementRecipeAction, self).__init__(source, source)

class AddIngredientAction(RecipeAction):
    def __init__(self, source, amount):
        super(AddIngredientAction, self).__init__(source, BLENDER)
        self.amount = amount
    def getAmount(self):
        return self.amount

class MixForAction(SingleElementRecipeAction):
    def __init__(self, seconds):
        super(MixForAction, self).__init__(MIX)
        self.seconds = seconds
    def getSeconds(self):
        return self.seconds

class ServeAction(SingleElementRecipeAction):
    def __init__(self):
        super(ServeAction, self).__init__(MIX)

class AddIceAction(SingleElementRecipeAction):
    def __init__(self):
        super(AddIceAction, self).__init__(ADD_ICE)

class AgeAction(SingleElementRecipeAction):
    def __init__(self):
        super(AgeAction, self).__init__(AGE)

class SelectSlotAction(SingleElementRecipeAction):
    pass

class ResetAction(SingleElementRecipeAction):
    def __init__(self):
        super(ResetAction, self).__init__(RESET)

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

def dragAndDropTo(source, destination, use_shortcut):
    shortcut = source.getShortcut()
    if use_shortcut and shortcut != '\x00':
        return TypeCommand(shortcut)
    return DragAndDropCommand(source.getCentroid(), destination.getCentroid())

def trigger(screen_element, use_shortcut):
    shortcut = screen_element.getShortcut()
    if use_shortcut and shortcut != '\x00':
        return TypeCommand(shortcut)
    return ClickCommand(screen_element.getCentroid())

def nextActionFromDrinkRecipe(drink_recipe):
    yield AddIngredientAction(ADELHYDE, drink_recipe.adelhyde)
    yield AddIngredientAction(BRONSON_EXTRACT, drink_recipe.bronsonExtract)
    yield AddIngredientAction(POWDERED_DELTA, drink_recipe.powderedDelta)
    yield AddIngredientAction(FLANERGIDE, drink_recipe.flanergide)
    yield AddIngredientAction(KARMOTRINE, drink_recipe.karmotrine)
    if drink_recipe.addIce:
        yield AddIceAction()
    if drink_recipe.age:
        yield AgeAction()
    yield MixForAction(5 if drink_recipe.blend else 1)

def nextAction(drink_recipe, slot, serve, reset):
    if reset:
        yield ResetAction()
    yield SelectSlotAction(slot)
    for action in nextActionFromDrinkRecipe(drink_recipe):
        yield action
    if serve:
        yield ServeAction()

def nextCommandFromAction(screen_elements, use_shortcut, action):
    if isinstance(action, AddIngredientAction):
        for _ in range(action.getAmount()):
            yield dragAndDropTo(screen_elements[action.getSource()], screen_elements[action.getDestination()], use_shortcut)
    elif isinstance(action, MixForAction):
        yield trigger(screen_elements[action.getSource()], use_shortcut)
        yield WaitCommand(action.getSeconds())
        yield trigger(screen_elements[action.getSource()], use_shortcut)
    elif isinstance(action, SingleElementRecipeAction):
        yield trigger(screen_elements[action.getSource()], use_shortcut)
    else:
        print('Unexpected recipe action type:', action.__class__.__name__)

def execute(command):
    if isinstance(command, ClickCommand):
        source_x, source_y = command.source
        print('mousemove {source_x} {source_y} '.format(source_x=source_x, source_y=source_y), end='')
        print('sleep 0.5 ', end='')
        print('mousedown 1 ', end='')
        print('sleep 0.5 ', end='')
        print('mouseup 1 ', end='')
        print('sleep 0.5 ', end='')
    elif isinstance(command, DragAndDropCommand):
        source_x, source_y = command.source
        destination_x, destination_y = command.destination
        print('mousemove {source_x} {source_y} '.format(source_x=source_x, source_y=source_y), end='')
        print('sleep 0.5 ', end='')
        print('mousedown 1 ', end='')
        print('mousemove {destination_x} {destination_y} '.format(destination_x=destination_x, destination_y=destination_y), end='')
        print('sleep 0.5 ', end='')
        print('mouseup 1 ', end='')
    elif isinstance(command, WaitCommand):
        print('sleep {seconds} '.format(seconds=command.seconds), end='')
    elif isinstance(command, TypeCommand):
        print('key {shortcut} '.format(shortcut=command.shortcut), end='')
    else:
        print('Unexpected command type:', command.__class__.__name__)

def getDrinkRecipe(request):
    drink_recipe = Recipe(drink[DrinkName._VALUES_TO_NAMES[request.drinkName]][RECIPE])

    if request.bigSize and request.drinkName == CREVICE_SPIKE:
        print('Adding karmotrine to big crevice spike.')
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

def getCommands(request):
    drink_recipe_request = DrinkRecipeRequest(
                               drinkName=request.drinkName,
                               addKarmotrine=request.addKarmotrine,
                               bigSize=request.bigSize)

    drink_recipe_response = getDrinkRecipe(drink_recipe_request)

    screen_elements = dict()
    for name in Recipe.ingredients:
        screen_elements[name] = ScreenElement(centroid[name])
    for name in ScreenElement.elements:
        screen_elements[name] = ScreenElement(centroid[name])

    for action in nextAction(
                      drink_recipe_response.drinkRecipe,
                      ScreenElementType._VALUES_TO_NAMES[request.slot],
                      request.serve,
                      request.reset):
        for command in nextCommandFromAction(
                           screen_elements, request.useShortcut, action):
            yield command

def main():
    #add_opt = True
    add_opt = False
    #serve = True
    serve = False
    slot = ScreenElementType.LEFT_SLOT
    #slot = RIGHT_SLOT
    #double = True
    double = False
    #use_shortcut = True
    use_shortcut = False
    reset = True
    #reset = False

    #drink_name = BAD_TOUCH
    drink_name = DrinkName.BEER
    #drink_name = BLEEDING_JANE
    #drink_name = BLOOM_LIGHT
    #drink_name = BLUE_FAIRY
    #drink_name = BRANDTINI
    #drink_name = COBALT_VELVET
    #drink_name = CREVICE_SPIKE
    #drink_name = FLUFFY_DREAM
    #drink_name = FRINGE_WEAVER
    #drink_name = FROTHY_WATER
    #drink_name = GRIZZLY_TEMPLE
    #drink_name = GUT_PUNCH
    #drink_name = MARSBLAST
    #drink_name = MERCURYBLAST
    #drink_name = MOONBLAST
    #drink_name = PIANO_MAN
    #drink_name = PIANO_WOMAN
    #drink_name = PILEDRIVER
    #drink_name = SPARKLE_STAR
    #drink_name = SUGAR_RUSH
    #drink_name = SUNSHINE_CLOUD
    #drink_name = SUPLEX
    #drink_name = ZEN_STAR
    #drink_name = FLAMING_MOAI

    command_request = CommandRequest(
                          drinkName=drink_name,
                          addKarmotrine=add_opt,
                          bigSize=double,
                          reset=reset,
                          slot=slot,
                          serve=serve,
                          useShortcut=use_shortcut)

    print('xdotool search --name "VA-11 Hall-A: Cyberpunk Bartender Action" ', end='')
    for command in getCommands(command_request):
        execute(command)
    print()

if __name__ == '__main__':
    main()
