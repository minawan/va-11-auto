import os

Settings.MoveMouseDelay = 0.1
Settings.DelayBeforeMouseDown = 0.1
Settings.DelayBeforeDrag = 0.1
Settings.DelayBeforeDrop = 0.1

resource_location = os.environ['VALHALLA_ROOT']
centroid_filename = os.path.join(resource_location, 'centroid.py')
drink_filename = os.path.join(resource_location, 'drink.py')

with open(centroid_filename, 'r') as centroid_file:
    exec(centroid_file.read())
with open(drink_filename, 'r') as drink_file:
    exec(drink_file.read())

class DragAndDropCommand:
    def __init__(self, source, destination):
        self.source = source
        self.destination = destination
    def execute(self):
        dragDrop(Location(*self.source), Location(*self.destination))

class WaitCommand:
    def __init__(self, seconds):
        self.seconds = seconds
    def execute(self):
        wait(self.seconds)

class TypeCommand:
    def __init__(self, shortcut):
        self.shortcut = shortcut
    def execute(self):
        type(self.shortcut)

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
    def __init__(self, source):
        super(AddIngredientAction, self).__init__(source, BLENDER)

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
    def __init__(self, category, name):
        entry = centroid[category][name]
        x = entry[X_COORD]
        y = entry[Y_COORD]
        self.centroid = (x, y)
        try:
            self.shortcut = str(chr(entry[SHORTCUT]))
        except KeyError:
            print('Shortcut not available for {name} of type {category}'.format(name=name, category=category))
            self.shortcut = 'l'

    def getCentroid(self):
        return self.centroid

    def getShortcut(self):
        return self.shortcut

class Recipe:
    def __init__(self, recipe):
        self.recipe = recipe

    def _isBig(self, ingredients):
        total_amount = 0
        for name in ingredients:
            individual_amount = self.recipe[name]
            if individual_amount > 0:
                total_amount += individual_amount
        return total_amount > 10

    def addOpt(self, ingredients):
        for name in ingredients:
            if self.recipe[name] < 0:
                self.recipe[name] = 1

    def doubleSize(self, ingredients):
        if not self._isBig(ingredients):
            for name in ingredients:
                self.recipe[name] *= 2

    def getIngredientCount(self, ingredient_name):
        return self.recipe[ingredient_name]

    def isOnTheRocks(self):
        return self.recipe[ADD_ICE]

    def isAged(self):
        return self.recipe[AGE]

    def mixDuration(self):
        return 5 if self.recipe[WAIT] else 1

    def nextAction(self, ingredients):
        for ingredient_name in ingredients:
            for _ in range(self.getIngredientCount(ingredient_name)):
                yield AddIngredientAction(ingredient_name)
        if self.isOnTheRocks():
            yield AddIceAction()
        if self.isAged():
            yield AgeAction()
        yield MixForAction(self.mixDuration())

def dragAndDropTo(source, destination, use_shortcut):
    shortcut = source.getShortcut()
    if use_shortcut and shortcut != '\x00':
        return TypeCommand(shortcut)
    return DragAndDropCommand(source.getCentroid(), destination.getCentroid())

def trigger(screen_element, use_shortcut):
    return dragAndDropTo(screen_element, screen_element, use_shortcut)

def nextCommand(recipe, ingredients, ingredient, button, slot, blender, serve, use_shortcut):
    yield trigger(button[slot], use_shortcut)
    yield trigger(button[RESET], use_shortcut)

    for action in recipe.nextAction(ingredients):
        if isinstance(action, AddIngredientAction):
            yield dragAndDropTo(ingredient[action.getSource()], blender, use_shortcut)
        elif isinstance(action, AddIceAction):
            yield trigger(button[action.getSource()], use_shortcut)
        elif isinstance(action, AgeAction):
            yield trigger(button[action.getSource()], use_shortcut)
        elif isinstance(action, MixForAction):
            yield trigger(button[action.getSource()], use_shortcut)
            yield WaitCommand(action.getSeconds())
            yield trigger(button[action.getSource()], use_shortcut)
        else:
            print('Unexpected recipe action type:', action.__class__.__name__)

    if serve:
        yield trigger(button[MIX], use_shortcut)

ingredients = [ADELHYDE, BRONSON_EXTRACT, POWDERED_DELTA, FLANERGIDE, KARMOTRINE]
buttons = [ADD_ICE, AGE, LEFT_SLOT, RIGHT_SLOT, RESET, MIX]

ingredient_element = { name: ScreenElement(INGREDIENT, name) for name in ingredients }
button_element = { name: ScreenElement(BUTTON, name) for name in buttons }
blender = ScreenElement(OTHER, BLENDER)

add_opt = True
#add_opt = False
#serve = True
serve = False
#slot = LEFT_SLOT
slot = RIGHT_SLOT
#double = True
double = False
#use_shortcut = True
use_shortcut = False

#drink_name = BAD_TOUCH
drink_name = BEER
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

if double and not add_opt and drink_name == CREVICE_SPIKE:
    print('Adding karmotrine to big crevice spike.')
    add_opt = True

drink_recipe = { name: Recipe(attr[RECIPE]) for name, attr in drink.items() }

if double:
    drink_recipe[drink_name].doubleSize(ingredients)

if add_opt:
    drink_recipe[drink_name].addOpt(ingredients)

for command in nextCommand(drink_recipe[drink_name], ingredients, ingredient_element, button_element, slot, blender, serve, use_shortcut):
    command.execute()
