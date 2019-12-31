import os

resource_location = os.environ['VALHALLA_ROOT']
centroid_filename = os.path.join(resource_location, 'centroid.py')
drink_filename = os.path.join(resource_location, 'drink.py')

with open(centroid_filename, 'r') as centroid_file:
    exec(centroid_file.read())
with open(drink_filename, 'r') as drink_file:
    exec(drink_file.read())

class ScreenElement:
    def __init__(self, category, name, use_shortcut=False):
        entry = centroid[category][name]
        x = entry[X]
        y = entry[Y]
        self.centroid = (x, y)
        self.use_shortcut = use_shortcut
        try:
            self.shortcut = entry[SHORTCUT]
        except KeyError:
            print('Shortcut not available for {name} of type {category}'.format(name=name, category=category))
            self.shortcut = 'l'
    def click(self):
        self.dragDrop(self)
    def dragDrop(self, element):
        dragDrop(Location(*self.centroid), Location(*element.centroid))
    def trigger(self):
        if self.use_shortcut:
            wait(0.3)
            type(self.shortcut)
        else:
            self.click()
    # jython pls why no overload
    def trigger_with_arg(self, element):
        if self.use_shortcut:
            wait(0.3)
            type(self.shortcut)
        else:
            self.dragDrop(element)

class Recipe:
    def __init__(self, recipe, add_opt, dry_run):
        self.recipe = recipe
        self.add_opt = add_opt
        self.dry_run = dry_run
    def apply(self, ingredient, button, slot, blender, double=False):
        button[slot].trigger()
        button[RESET].trigger()
        for name, screen_element in ingredient.items():
            if self.recipe[name] < 0 and self.add_opt:
                screen_element.trigger_with_arg(blender)
            else:
                max_count = 2 * self.recipe[name] if double else self.recipe[name] 
                for _ in range(max_count):
                    screen_element.trigger_with_arg(blender)
        if self.recipe[ADD_ICE]:
            button[ADD_ICE].trigger()
        if self.recipe[AGE]:
            button[AGE].trigger()
        button[MIX].trigger()
        wait(5 if self.recipe[WAIT] else 1)
        button[MIX].trigger()
        if self.dry_run:
            pass
            #button[RESET].trigger()
        else:
            button[MIX].trigger()

ingredients = [ADELHYDE, BRONSON_EXTRACT, POWDERED_DELTA, FLANERGIDE, KARMOTRINE]
buttons = [ADD_ICE, AGE, LEFT_SLOT, RIGHT_SLOT, RESET, MIX]

ingredient_element = { name: ScreenElement(INGREDIENT, name) for name in ingredients }
button_element = { name: ScreenElement(BUTTON, name) for name in buttons }
blender = ScreenElement(OTHER, BLENDER)

add_opt = True
#add_opt = False
dry_run = True
#dry_run = False
slot = LEFT_SLOT
#slot = RIGHT_SLOT
#double = True
double = False
Settings.MoveMouseDelay = 0.1
Settings.DelayBeforeMouseDown = 0.1
Settings.DelayBeforeDrag = 0.1
Settings.DelayBeforeDrop = 0.1

#drink_name = BAD_TOUCH
#drink_name = BEER
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
drink_name = SUGAR_RUSH
#drink_name = SUNSHINE_CLOUD
#drink_name = SUPLEX
#drink_name = ZEN_STAR
#drink_name = FLAMING_MOAI

if double and drink_name in [MARSBLAST, PIANO_MAN, PIANO_WOMAN, ZEN_STAR, FLAMING_MOAI]:
    print('{drink_name} is already big.'.format(drink_name=drink_name))
    double = False

if double and not add_opt and drink_name == CREVICE_SPIKE:
    print('Adding karmotrine to big crevice spike.')
    add_opt = True

drink_recipe = { name: Recipe(attr[RECIPE], add_opt, dry_run) for name, attr in drink.items() }

drink_recipe[drink_name].apply(ingredient_element, button_element, slot, blender, double=double)
#wait(5)

