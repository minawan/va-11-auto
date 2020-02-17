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
    def __init__(self, category, name, use_shortcut=False):
        entry = centroid[category][name]
        x = entry[X_COORD]
        y = entry[Y_COORD]
        self.centroid = (x, y)
        self.use_shortcut = use_shortcut
        try:
            self.shortcut = entry[SHORTCUT]
        except KeyError:
            print('Shortcut not available for {name} of type {category}'.format(name=name, category=category))
            self.shortcut = 'l'

    def click(self):
        return self.dragDrop(self)

    def dragDrop(self, element):
        return DragAndDropCommand(self.centroid, element.centroid)

    def trigger(self):
        if self.use_shortcut:
            yield WaitCommand(0.3)
            yield TypeCommand(self.shortcut)
        else:
            yield self.click()

    # jython pls why no overload
    def trigger_with_arg(self, element):
        if self.use_shortcut:
            yield WaitCommand(0.3)
            yield TypeCommand(self.shortcut)
        else:
            yield self.dragDrop(element)

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

def nextCommand(recipe, ingredient, button, slot, blender, serve):
    for cmd in button[slot].trigger():
        yield cmd
    for cmd in button[RESET].trigger():
        yield cmd

    for name, screen_element in ingredient.items():
        for _ in range(recipe.getIngredientCount(name)):
            for cmd in screen_element.trigger_with_arg(blender):
                yield cmd

    if recipe.isOnTheRocks():
        for cmd in button[ADD_ICE].trigger():
            yield cmd

    if recipe.isAged():
        for cmd in button[AGE].trigger():
            yield cmd

    for cmd in button[MIX].trigger():
        yield cmd
    yield WaitCommand(recipe.mixDuration())
    for cmd in button[MIX].trigger():
        yield cmd

    if serve:
        for cmd in button[MIX].trigger():
            yield cmd

ingredients = [ADELHYDE, BRONSON_EXTRACT, POWDERED_DELTA, FLANERGIDE, KARMOTRINE]
buttons = [ADD_ICE, AGE, LEFT_SLOT, RIGHT_SLOT, RESET, MIX]

ingredient_element = { name: ScreenElement(INGREDIENT, name) for name in ingredients }
button_element = { name: ScreenElement(BUTTON, name) for name in buttons }
blender = ScreenElement(OTHER, BLENDER)

#add_opt = True
add_opt = False
#serve = True
serve = False
slot = LEFT_SLOT
#slot = RIGHT_SLOT
double = True
#double = False

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

for command in nextCommand(drink_recipe[drink_name], ingredient_element, button_element, slot, blender, serve):
    command.execute()
