import sys
import os

sys.path.append('thrift/gen-py')

from action.ttypes import AddIceAction
from action.ttypes import AddIngredientAction
from action.ttypes import AgeAction
from action.ttypes import MixAction
from action.ttypes import RecipeActionRequest
from action.ttypes import RecipeActionResponse
from action.ttypes import RecipeActionSet
from action.ttypes import ResetAction
from action.ttypes import ScreenElementType
from action.ttypes import SelectSlotAction
from action.ttypes import ServeAction
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

def getRecipeActions(request):
    actions = RecipeActionSet()
    if request.reset:
        actions.resetAction = ResetAction()
    actions.selectSlotAction = SelectSlotAction(slot=request.slot)
    drink_recipe = request.drinkRecipe
    actions.addIngredientActions = []
    actions.addIngredientActions.append(AddIngredientAction(ingredient=ScreenElementType.ADELHYDE, amount=drink_recipe.adelhyde))
    actions.addIngredientActions.append(AddIngredientAction(ingredient=ScreenElementType.BRONSON_EXTRACT, amount=drink_recipe.bronsonExtract))
    actions.addIngredientActions.append(AddIngredientAction(ingredient=ScreenElementType.POWDERED_DELTA, amount=drink_recipe.powderedDelta))
    actions.addIngredientActions.append(AddIngredientAction(ingredient=ScreenElementType.FLANERGIDE, amount=drink_recipe.flanergide))
    actions.addIngredientActions.append(AddIngredientAction(ingredient=ScreenElementType.KARMOTRINE, amount=drink_recipe.karmotrine))
    if drink_recipe.addIce:
        actions.addIceAction = AddIceAction()
    if drink_recipe.age:
        actions.ageAction = AgeAction()
    durationInSeconds = 5 if drink_recipe.blend else 1
    actions.mixAction = MixAction(durationInSeconds=durationInSeconds)
    if request.serve:
        actions.serveAction = ServeAction()

    return RecipeActionResponse(recipeActions=actions)

def nextAction(drink_recipe, slot, serve, reset):
    request = RecipeActionRequest(
                  drinkRecipe=drink_recipe,
                  reset=reset,
                  slot=slot,
                  serve=serve)
    response = getRecipeActions(request)
    actions = response.recipeActions
    if actions.resetAction:
        yield actions.resetAction
    if actions.selectSlotAction:
        yield actions.selectSlotAction
    for add_ingredient_action in actions.addIngredientActions:
        yield add_ingredient_action
    if actions.addIceAction:
        yield actions.addIceAction
    if actions.ageAction:
        yield actions.ageAction
    yield actions.mixAction
    if actions.serveAction:
        yield actions.serveAction

def nextCommandFromAction(screen_elements, use_shortcut, action):
    if isinstance(action, AddIngredientAction):
        for _ in range(action.amount):
            yield dragAndDropTo(screen_elements[ScreenElementType._VALUES_TO_NAMES[action.ingredient]], screen_elements[BLENDER], use_shortcut)
    elif isinstance(action, MixAction):
        yield trigger(screen_elements[MIX], use_shortcut)
        yield WaitCommand(action.durationInSeconds)
        yield trigger(screen_elements[MIX], use_shortcut)
    elif isinstance(action, AddIceAction):
        yield trigger(screen_elements[ADD_ICE], use_shortcut)
    elif isinstance(action, AgeAction):
        yield trigger(screen_elements[AGE], use_shortcut)
    elif isinstance(action, ResetAction):
        yield trigger(screen_elements[RESET], use_shortcut)
    elif isinstance(action, SelectSlotAction):
        yield trigger(screen_elements[ScreenElementType._VALUES_TO_NAMES[action.slot]], use_shortcut)
    elif isinstance(action, ServeAction):
        yield trigger(screen_elements[MIX], use_shortcut)
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
                      request.slot,
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
