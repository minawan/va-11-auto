# Category
BUTTON = 'Button'
INGREDIENT = 'Ingredient'
OTHER = 'Other'

# Button
ADDICE = 'AddIce'
AGE = 'Age'
LEFTSLOT = 'LeftSlot'
RIGHTSLOT = 'RightSlot'
RESET = 'Reset'
MIX = 'Mix'

# Ingredient
ADELHYDE = 'Adelhyde'
BRONSONEXTRACT = 'BronsonExtract'
POWDEREDDELTA = 'PowderedDelta'
FLANERGIDE = 'Flanergide'
KARMOTRINE = 'Karmotrine'

# Other
BLENDER = 'Blender'
SHORTCUT = 'Shortcut'

# Coordinate
X = 'X'
Y = 'Y'

centroid = {
        BUTTON: {
            ADDICE: {
                X: 759,
                Y: 340,
                SHORTCUT: 'a'
                },
            AGE: {
                X: 760,
                Y: 473,
                SHORTCUT: 's',
                },
            LEFTSLOT: {
                X: 1026,
                Y: 242,
                },
            RIGHTSLOT: {
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
            BRONSONEXTRACT: {
                X: 984,
                Y: 345,
                SHORTCUT: 'w',
                },
            POWDEREDDELTA: {
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
