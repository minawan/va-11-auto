# Category
BUTTON = 'button'
INGREDIENT = 'ingredient'
OTHER = 'other'

# Button
ICE = 'ice'
AGE = 'age'
LEFT_SLOT = 'left_slot'
RIGHT_SLOT = 'right_slot'
RESET = 'reset'
MIX = 'mix'

# Ingredient
ADELHYDE = 'adelhyde'
BRONSON_EXTRACT = 'bronson_extract'
POWDERED_DELTA = 'powdered_delta'
FLANERGIDE = 'flanergide'
KARMOTRINE = 'karmotrine'

# Other
BLENDER = 'blender'
SHORTCUT = 'shortcut'

# Coordinate
X = 'x'
Y = 'y'

# Drink attribute
FLAVOR = 'flavor'
KIND = 'kind'
PRICE = 'price'
RECIPE = 'recipe'
TRAIT = 'trait'

# Drink
A_FEDORA_WITH_PERFUME_AND_A_PLUM = 'A Fedora “w/ perfume & a plum”'
BAD_TOUCH = 'Bad Touch'
BEER = 'Beer'
BLEEDING_JANE = 'Bleeding Jane'
BLOOM_LIGHT = 'Bloom Light'
BLUE_FAIRY = 'Blue Fairy'
BRANDTINI = 'Brandtini'
COBALT_VELVET = 'Cobalt Velvet'
CREVICE_SPIKE = 'Crevice Spike'
FLUFFY_DREAM = 'Fluffy Dream'
FRINGE_WEAVER = 'Fringe Weaver'
FROTHY_WATER = 'Frothy Water'
GRIZZLY_TEMPLE = 'Grizzly Temple'
GUT_PUNCH = 'Gut Punch'
MARSBLAST = 'Marsblast'
MERCURYBLAST = 'Mercuryblast'
MOONBLAST = 'Moonblast'
PIANO_MAN = 'Piano Man'
PIANO_WOMAN = 'Piano Woman'
PILEDRIVER = 'Piledriver'
SPARKLE_STAR = 'Sparkle Star'
SUGAR_RUSH = 'Sugar Rush'
SUNSHINE_CLOUD = 'Sunshine Cloud'
SUPLEX = 'Suplex'
ZEN_STAR = 'Zen Star'

# Flavor
BITTER = 'Bitter'
BUBBLY = 'Bubbly'
NONE = ''
SOUR = 'Sour'
SPICY = 'Spicy'
SWEET = 'Sweet'

# Kind
CLASSIC = 'Classic'
CLASSY = 'Classy'
GIRLY = 'Girly'
MANLY = 'Manly'
#NONE = ''
PROMO = 'Promo'

# Recipe action
#ICE = 'ice'
#AGE = 'age'
#MIX = 'mix
BLEND = 'blend'

# Trait
BURNING = 'Burning'
BLAND = 'Bland'
HAPPY = 'Happy'
#NONE = ''
SOBERING = 'Sobering'
SOFT = 'Soft'
STRONG = 'Strong'
VINTAGE = 'Vintage'

centroid = {
        BUTTON: {
            ICE: {
                X: 759,
                Y: 340,
                SHORTCUT: 'a'
                },
            AGE: {
                X: 760,
                Y: 473,
                SHORTCUT: 's',
                },
            LEFT_SLOT: {
                X: 1026,
                Y: 242,
                },
            RIGHT_SLOT: {
                X: 1177,
                Y: 244,
                },
            RESET: {
                X: 875,
                Y: 541,
                },
            MIX: {
                X: 1180,
                Y: 542,
                SHORTCUT: Key.SPACE,
                },
            },
        INGREDIENT: {
            ADELHYDE: {
                X: 834,
                Y: 345,
                SHORTCUT: 'q',
                },
            BRONSON_EXTRACT: {
                X: 984,
                Y: 345,
                SHORTCUT: 'w',
                },
            POWDERED_DELTA: {
                X: 1133,
                Y: 345,
                SHORTCUT: 'e',
                },
            FLANERGIDE: {
                X: 835,
                Y: 467,
                SHORTCUT: 'r',
                },
            KARMOTRINE: {
                X: 1135,
                Y: 466,
                SHORTCUT: 't',
                },
            },
        OTHER: {
            BLENDER: {
                X: 1026,
                Y: 458,
                },
            },
        }

drink = {A_FEDORA_WITH_PERFUME_AND_A_PLUM: {FLAVOR: NONE,
                                    KIND: NONE,
                                    PRICE: 500,
                                    RECIPE: {ADELHYDE: 0,
                                               AGE: False,
                                               BLEND: False,
                                               BRONSON_EXTRACT: 0,
                                               FLANERGIDE: 0,
                                               ICE: False,
                                               KARMOTRINE: 0,
                                               MIX: False,
                                               POWDERED_DELTA: 0},
                                    TRAIT: NONE},
 BAD_TOUCH: {FLAVOR: SOUR,
               KIND: CLASSY,
               PRICE: 250,
               RECIPE: {ADELHYDE: 0,
                          AGE: False,
                          BLEND: False,
                          BRONSON_EXTRACT: 2,
                          FLANERGIDE: 2,
                          ICE: True,
                          KARMOTRINE: 4,
                          MIX: True,
                          POWDERED_DELTA: 2},
               TRAIT: VINTAGE},
 BEER: {FLAVOR: BUBBLY,
          KIND: CLASSIC,
          PRICE: 200,
          RECIPE: {ADELHYDE: 1,
                     AGE: False,
                     BLEND: False,
                     BRONSON_EXTRACT: 2,
                     FLANERGIDE: 2,
                     ICE: False,
                     KARMOTRINE: 4,
                     MIX: True,
                     POWDERED_DELTA: 1},
          TRAIT: VINTAGE},
 BLEEDING_JANE: {FLAVOR: SPICY,
                   KIND: CLASSIC,
                   PRICE: 200,
                   RECIPE: {ADELHYDE: 0,
                              AGE: False,
                              BLEND: True,
                              BRONSON_EXTRACT: 1,
                              FLANERGIDE: 3,
                              ICE: False,
                              KARMOTRINE: 0,
                              MIX: False,
                              POWDERED_DELTA: 3},
                   TRAIT: SOBERING},
 BLOOM_LIGHT: {FLAVOR: SPICY,
                 KIND: PROMO,
                 PRICE: 230,
                 RECIPE: {ADELHYDE: 4,
                            AGE: True,
                            BLEND: False,
                            BRONSON_EXTRACT: 0,
                            FLANERGIDE: 2,
                            ICE: True,
                            KARMOTRINE: 3,
                            MIX: True,
                            POWDERED_DELTA: 1},
                 TRAIT: BLAND},
 BLUE_FAIRY: {FLAVOR: SWEET,
                KIND: GIRLY,
                PRICE: 170,
                RECIPE: {ADELHYDE: 4,
                           AGE: True,
                           BLEND: False,
                           BRONSON_EXTRACT: 0,
                           FLANERGIDE: 1,
                           ICE: False,
                           KARMOTRINE: -1,
                           MIX: True,
                           POWDERED_DELTA: 0},
                TRAIT: SOFT},
 BRANDTINI: {FLAVOR: SWEET,
               KIND: CLASSY,
               PRICE: 250,
               RECIPE: {ADELHYDE: 6,
                          AGE: True,
                          BLEND: False,
                          BRONSON_EXTRACT: 0,
                          FLANERGIDE: 0,
                          ICE: False,
                          KARMOTRINE: 1,
                          MIX: True,
                          POWDERED_DELTA: 3},
               TRAIT: HAPPY},
 COBALT_VELVET: {FLAVOR: BUBBLY,
                   KIND: CLASSY,
                   PRICE: 280,
                   RECIPE: {ADELHYDE: 2,
                              AGE: False,
                              BLEND: False,
                              BRONSON_EXTRACT: 0,
                              FLANERGIDE: 3,
                              ICE: True,
                              KARMOTRINE: 5,
                              MIX: True,
                              POWDERED_DELTA: 0},
                   TRAIT: BURNING},
 CREVICE_SPIKE: {FLAVOR: SOUR,
                   KIND: MANLY,
                   PRICE: 140,
                   RECIPE: {ADELHYDE: 0,
                              AGE: False,
                              BLEND: True,
                              BRONSON_EXTRACT: 0,
                              FLANERGIDE: 4,
                              ICE: False,
                              KARMOTRINE: -1,
                              MIX: False,
                              POWDERED_DELTA: 2},
                   TRAIT: SOBERING},
 FLUFFY_DREAM: {FLAVOR: SOUR,
                  KIND: GIRLY,
                  PRICE: 170,
                  RECIPE: {ADELHYDE: 3,
                             AGE: True,
                             BLEND: False,
                             BRONSON_EXTRACT: 0,
                             FLANERGIDE: 0,
                             ICE: False,
                             KARMOTRINE: -1,
                             MIX: True,
                             POWDERED_DELTA: 3},
                  TRAIT: SOFT},
 FRINGE_WEAVER: {FLAVOR: BUBBLY,
                   KIND: CLASSY,
                   PRICE: 260,
                   RECIPE: {ADELHYDE: 1,
                              AGE: True,
                              BLEND: False,
                              BRONSON_EXTRACT: 0,
                              FLANERGIDE: 0,
                              ICE: False,
                              KARMOTRINE: 9,
                              MIX: True,
                              POWDERED_DELTA: 0},
                   TRAIT: STRONG},
 FROTHY_WATER: {FLAVOR: BUBBLY,
                  KIND: CLASSIC,
                  PRICE: 150,
                  RECIPE: {ADELHYDE: 1,
                             AGE: True,
                             BLEND: False,
                             BRONSON_EXTRACT: 1,
                             FLANERGIDE: 1,
                             ICE: False,
                             KARMOTRINE: 0,
                             MIX: True,
                             POWDERED_DELTA: 1},
                  TRAIT: BLAND},
 GRIZZLY_TEMPLE: {FLAVOR: BITTER,
                    KIND: PROMO,
                    PRICE: 220,
                    RECIPE: {ADELHYDE: 3,
                               AGE: False,
                               BLEND: True,
                               BRONSON_EXTRACT: 3,
                               FLANERGIDE: 0,
                               ICE: False,
                               KARMOTRINE: 1,
                               MIX: False,
                               POWDERED_DELTA: 3},
                    TRAIT: BLAND},
 GUT_PUNCH: {FLAVOR: BITTER,
               KIND: MANLY,
               PRICE: 80,
               RECIPE: {ADELHYDE: 0,
                          AGE: True,
                          BLEND: False,
                          BRONSON_EXTRACT: 5,
                          FLANERGIDE: 1,
                          ICE: False,
                          KARMOTRINE: -1,
                          MIX: True,
                          POWDERED_DELTA: 0},
               TRAIT: STRONG},
 MARSBLAST: {FLAVOR: SPICY,
               KIND: MANLY,
               PRICE: 170,
               RECIPE: {ADELHYDE: 0,
                          AGE: False,
                          BLEND: True,
                          BRONSON_EXTRACT: 6,
                          FLANERGIDE: 4,
                          ICE: False,
                          KARMOTRINE: 2,
                          MIX: False,
                          POWDERED_DELTA: 1},
               TRAIT: STRONG},
 MERCURYBLAST: {FLAVOR: SOUR,
                  KIND: CLASSY,
                  PRICE: 250,
                  RECIPE: {ADELHYDE: 1,
                             AGE: False,
                             BLEND: True,
                             BRONSON_EXTRACT: 1,
                             FLANERGIDE: 3,
                             ICE: True,
                             KARMOTRINE: 2,
                             MIX: False,
                             POWDERED_DELTA: 3},
                  TRAIT: BURNING},
 MOONBLAST: {FLAVOR: SWEET,
               KIND: GIRLY,
               PRICE: 180,
               RECIPE: {ADELHYDE: 6,
                          AGE: False,
                          BLEND: True,
                          BRONSON_EXTRACT: 0,
                          FLANERGIDE: 1,
                          ICE: True,
                          KARMOTRINE: 2,
                          MIX: False,
                          POWDERED_DELTA: 1},
               TRAIT: HAPPY},
 PIANO_MAN: {FLAVOR: SOUR,
               KIND: PROMO,
               PRICE: 320,
               RECIPE: {ADELHYDE: 2,
                          AGE: False,
                          BLEND: False,
                          BRONSON_EXTRACT: 3,
                          FLANERGIDE: 5,
                          ICE: True,
                          KARMOTRINE: 3,
                          MIX: True,
                          POWDERED_DELTA: 5},
               TRAIT: STRONG},
 PIANO_WOMAN: {FLAVOR: SWEET,
                 KIND: PROMO,
                 PRICE: 320,
                 RECIPE: {ADELHYDE: 5,
                            AGE: True,
                            BLEND: False,
                            BRONSON_EXTRACT: 5,
                            FLANERGIDE: 3,
                            ICE: False,
                            KARMOTRINE: 3,
                            MIX: True,
                            POWDERED_DELTA: 2},
                 TRAIT: HAPPY},
 PILEDRIVER: {FLAVOR: BITTER,
                KIND: MANLY,
                PRICE: 160,
                RECIPE: {ADELHYDE: 0,
                           AGE: False,
                           BLEND: False,
                           BRONSON_EXTRACT: 3,
                           FLANERGIDE: 3,
                           ICE: False,
                           KARMOTRINE: 4,
                           MIX: True,
                           POWDERED_DELTA: 0},
                TRAIT: BURNING},
 SPARKLE_STAR: {FLAVOR: SWEET,
                  KIND: GIRLY,
                  PRICE: 150,
                  RECIPE: {ADELHYDE: 2,
                             AGE: True,
                             BLEND: False,
                             BRONSON_EXTRACT: 0,
                             FLANERGIDE: 0,
                             ICE: False,
                             KARMOTRINE: -1,
                             MIX: True,
                             POWDERED_DELTA: 1},
                  TRAIT: HAPPY},
 SUGAR_RUSH: {FLAVOR: SWEET,
                KIND: GIRLY,
                PRICE: 150,
                RECIPE: {ADELHYDE: 2,
                           AGE: False,
                           BLEND: False,
                           BRONSON_EXTRACT: 0,
                           FLANERGIDE: 0,
                           ICE: False,
                           KARMOTRINE: -1,
                           MIX: True,
                           POWDERED_DELTA: 1},
                TRAIT: HAPPY},
 SUNSHINE_CLOUD: {FLAVOR: BITTER,
                    KIND: GIRLY,
                    PRICE: 150,
                    RECIPE: {ADELHYDE: 2,
                               AGE: False,
                               BLEND: True,
                               BRONSON_EXTRACT: 2,
                               FLANERGIDE: 0,
                               ICE: True,
                               KARMOTRINE: -1,
                               MIX: False,
                               POWDERED_DELTA: 0},
                    TRAIT: SOFT},
 SUPLEX: {FLAVOR: BITTER,
            KIND: MANLY,
            PRICE: 160,
            RECIPE: {ADELHYDE: 0,
                       AGE: False,
                       BLEND: False,
                       BRONSON_EXTRACT: 4,
                       FLANERGIDE: 3,
                       ICE: True,
                       KARMOTRINE: 3,
                       MIX: True,
                       POWDERED_DELTA: 0},
            TRAIT: BURNING},
 ZEN_STAR: {FLAVOR: SOUR,
              KIND: PROMO,
              PRICE: 210,
              RECIPE: {ADELHYDE: 4,
                         AGE: False,
                         BLEND: False,
                         BRONSON_EXTRACT: 4,
                         FLANERGIDE: 4,
                         ICE: True,
                         KARMOTRINE: 4,
                         MIX: True,
                         POWDERED_DELTA: 4},
              TRAIT: BLAND}}

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
        #slot.trigger()
        button[RESET].trigger()
        for name, screen_element in ingredient.items():
            if self.recipe[name] < 0 and self.add_opt:
                screen_element.trigger_with_arg(blender)
            else:
                max_count = 2 * self.recipe[name] if double else self.recipe[name] 
                for _ in range(max_count):
                    screen_element.trigger_with_arg(blender)
        if self.recipe[ICE]:
            button[ICE].trigger()
        if self.recipe[AGE]:
            button[AGE].trigger()
        button[MIX].trigger()
        wait(5 if self.recipe[BLEND] else 1)
        button[MIX].trigger()
        if self.dry_run:
            pass
            #button[RESET].trigger()
        else:
            button[MIX].trigger()

adelhyde = ScreenElement(INGREDIENT, ADELHYDE, use_shortcut=False)
bronson_extract = ScreenElement(INGREDIENT, BRONSON_EXTRACT, use_shortcut=False)
powdered_delta = ScreenElement(INGREDIENT, POWDERED_DELTA, use_shortcut=False)
flanergide = ScreenElement(INGREDIENT, FLANERGIDE, use_shortcut=False)
karmotrine = ScreenElement(INGREDIENT, KARMOTRINE, use_shortcut=False)

ice = ScreenElement(BUTTON, ICE, use_shortcut=False)
age = ScreenElement(BUTTON, AGE, use_shortcut=False)
left_slot = ScreenElement(BUTTON, LEFT_SLOT)
right_slot = ScreenElement(BUTTON, RIGHT_SLOT)
reset = ScreenElement(BUTTON, RESET)
mix = ScreenElement(BUTTON, MIX, use_shortcut=False)

ingredient = {
        ADELHYDE: adelhyde,
        BRONSON_EXTRACT: bronson_extract,
        POWDERED_DELTA: powdered_delta,
        FLANERGIDE: flanergide,
        KARMOTRINE: karmotrine,
        }
button = {
        ICE: ice,
        AGE: age,
        RESET: reset,
        MIX: mix,
        }

blender = ScreenElement(OTHER, BLENDER)

add_opt = True
#add_opt = False
dry_run = True
#dry_run = False
slot = left_slot
#slot = right_slot
double = True
#double = False
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

if double and drink_name in [MARSBLAST, PIANO_MAN, PIANO_WOMAN, ZEN_STAR]:
    print('{} is already big.'.format(drink_name))
    double = False

if double and not add_opt and drink_name == CREVICE_SPIKE:
    print('Adding karmotrine to big crevice spike.')
    add_opt = True

drink_recipe = dict()
for name, attr in drink.items():
    drink_recipe[name] = Recipe(attr[RECIPE], add_opt, dry_run)

drink_recipe[drink_name].apply(ingredient, button, slot, blender, double=double)
#wait(5)

