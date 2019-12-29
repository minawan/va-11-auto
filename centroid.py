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
