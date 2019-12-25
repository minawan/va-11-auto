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
                },
            AGE: {
                X: 760,
                Y: 473,
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
                },
            },
        INGREDIENT: {
            ADELHYDE: {
                X: 834,
                Y: 345,
                },
            BRONSON_EXTRACT: {
                X: 984,
                Y: 345,
                },
            POWDERED_DELTA: {
                X: 1133,
                Y: 345,
                },
            FLANERGIDE: {
                X: 835,
                Y: 467,
                },
            KARMOTRINE: {
                X: 1135,
                Y: 466,
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
    def __init__(self, category, name):
        coord = centroid[category][name]
        x = coord[X]
        y = coord[Y]
        self.centroid = (x, y)
    def click(self):
        self.dragDrop(self)
    def dragDrop(self, element):
        dragDrop(Location(*self.centroid), Location(*element.centroid))



class Recipe:
    def __init__(self, recipe, add_opt, dry_run):
        self.recipe = recipe
        self.add_opt = add_opt
        self.dry_run = dry_run
    def apply(self, ingredient, button, slot, blender):
        slot.click()
        for name, screen_element in ingredient.items():
            if self.recipe[name] < 0 and self.add_opt:
                screen_element.dragDrop(blender)
            else:
                for _ in range(self.recipe[name]):
                    screen_element.dragDrop(blender)
        if self.recipe[ICE]:
            button[ICE].click()
        if self.recipe[AGE]:
            button[AGE].click()
        button[MIX].click()
        wait(5 if self.recipe[BLEND] else 1)
        button[MIX].click()
        if self.dry_run:
            button[RESET].click()
        else:
            button[MIX].click()

adelhyde = ScreenElement(INGREDIENT, ADELHYDE)
bronson_extract = ScreenElement(INGREDIENT, BRONSON_EXTRACT)
powdered_delta = ScreenElement(INGREDIENT, POWDERED_DELTA)
flanergide = ScreenElement(INGREDIENT, FLANERGIDE)
karmotrine = ScreenElement(INGREDIENT, KARMOTRINE)

ice = ScreenElement(BUTTON, ICE)
age = ScreenElement(BUTTON, AGE)
left_slot = ScreenElement(BUTTON, LEFT_SLOT)
right_slot = ScreenElement(BUTTON, RIGHT_SLOT)
reset = ScreenElement(BUTTON, RESET)
mix = ScreenElement(BUTTON, MIX)

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
dry_run = True
slot = left_slot
#slot = right_slot
Settings.MoveMouseDelay = 0.1
Settings.DelayBeforeMouseDown = 0.1
Settings.DelayBeforeDrag = 0.1
Settings.DelayBeforeDrop = 0.1

drink_recipe = dict()
for name, attr in drink.items():
    drink_recipe[name] = Recipe(attr[RECIPE], add_opt, dry_run)

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
drink_name = GRIZZLY_TEMPLE
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

drink_recipe[drink_name].apply(ingredient, button, slot, blender)
#wait(5)

