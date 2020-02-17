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

def dragAndDropTo(source, destination, use_shortcut):
    shortcut = source.getShortcut()
    if use_shortcut and shortcut != '\x00':
        return TypeCommand(shortcut)
    return DragAndDropCommand(source.getCentroid(), destination.getCentroid())

def trigger(screen_element, use_shortcut):
    return dragAndDropTo(screen_element, screen_element, use_shortcut)

def nextCommand(recipe, ingredient, button, slot, blender, serve, use_shortcut):
    yield trigger(button[slot], use_shortcut)
    yield trigger(button[RESET], use_shortcut)

    for name, screen_element in ingredient.items():
        for _ in range(recipe.getIngredientCount(name)):
            yield dragAndDropTo(screen_element, blender, use_shortcut)

    if recipe.isOnTheRocks():
        yield trigger(button[ADD_ICE], use_shortcut)

    if recipe.isAged():
        yield trigger(button[AGE], use_shortcut)

    yield trigger(button[MIX], use_shortcut)
    yield WaitCommand(recipe.mixDuration())
    yield trigger(button[MIX], use_shortcut)

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
slot = LEFT_SLOT
#slot = RIGHT_SLOT
double = True
#double = False
use_shortcut = True
#use_shortcut = False

#drink_name = BAD_TOUCH
#drink_name = BEER
#drink_name = BLEEDING_JANE
#drink_name = BLOOM_LIGHT
#drink_name = BLUE_FAIRY
#drink_name = BRANDTINI
drink_name = COBALT_VELVET
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

for command in nextCommand(drink_recipe[drink_name], ingredient_element, button_element, slot, blender, serve, use_shortcut):
    command.execute()
