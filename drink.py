FLAVOR = 'FLAVOR'
KIND = 'KIND'
TRAIT = 'TRAIT'
PRICE = 'PRICE'
RECIPE = 'RECIPE'
ADELHYDE = 'ADELHYDE'
BRONSON_EXTRACT = 'BRONSON_EXTRACT'
POWDERED_DELTA = 'POWDERED_DELTA'
FLANERGIDE = 'FLANERGIDE'
KARMOTRINE = 'KARMOTRINE'
ADD_ICE = 'ADD_ICE'
AGE = 'AGE'
WAIT = 'WAIT'
BITTER = 'BITTER'
BUBBLY = 'BUBBLY'
SOUR = 'SOUR'
SPICY = 'SPICY'
SWEET = 'SWEET'
CLASSIC = 'CLASSIC'
CLASSY = 'CLASSY'
GIRLY = 'GIRLY'
MANLY = 'MANLY'
PROMO = 'PROMO'
BLAND = 'BLAND'
BURNING = 'BURNING'
HAPPY = 'HAPPY'
SOBERING = 'SOBERING'
SOFT = 'SOFT'
STRONG = 'STRONG'
VINTAGE = 'VINTAGE'
NONE = 'NONE'
BAD_TOUCH = 'BAD_TOUCH'
BEER = 'BEER'
BLEEDING_JANE = 'BLEEDING_JANE'
BLOOM_LIGHT = 'BLOOM_LIGHT'
BLUE_FAIRY = 'BLUE_FAIRY'
BRANDTINI = 'BRANDTINI'
COBALT_VELVET = 'COBALT_VELVET'
CREVICE_SPIKE = 'CREVICE_SPIKE'
FLUFFY_DREAM = 'FLUFFY_DREAM'
FRINGE_WEAVER = 'FRINGE_WEAVER'
FROTHY_WATER = 'FROTHY_WATER'
GRIZZLY_TEMPLE = 'GRIZZLY_TEMPLE'
GUT_PUNCH = 'GUT_PUNCH'
MARSBLAST = 'MARSBLAST'
MERCURYBLAST = 'MERCURYBLAST'
MOONBLAST = 'MOONBLAST'
PIANO_MAN = 'PIANO_MAN'
PIANO_WOMAN = 'PIANO_WOMAN'
PILEDRIVER = 'PILEDRIVER'
SPARKLE_STAR = 'SPARKLE_STAR'
SUGAR_RUSH = 'SUGAR_RUSH'
SUNSHINE_CLOUD = 'SUNSHINE_CLOUD'
SUPLEX = 'SUPLEX'
ZEN_STAR = 'ZEN_STAR'
A_FEDORA_WITH_PERFUME_AND_A_PLUM = 'A_FEDORA_WITH_PERFUME_AND_A_PLUM'
FLAMING_MOAI = 'FLAMING_MOAI'
drink = {
  BAD_TOUCH: {
    FLAVOR: SOUR, 
    KIND: CLASSY, 
    TRAIT: VINTAGE, 
    PRICE: 250, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 2, 
      POWDERED_DELTA: 2, 
      FLANERGIDE: 2, 
      KARMOTRINE: 4, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  BEER: {
    FLAVOR: BUBBLY, 
    KIND: CLASSIC, 
    TRAIT: VINTAGE, 
    PRICE: 200, 
    RECIPE: {
      ADELHYDE: 1, 
      BRONSON_EXTRACT: 2, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 2, 
      KARMOTRINE: 4, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  BLEEDING_JANE: {
    FLAVOR: SPICY, 
    KIND: CLASSIC, 
    TRAIT: SOBERING, 
    PRICE: 200, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 1, 
      POWDERED_DELTA: 3, 
      FLANERGIDE: 3, 
      KARMOTRINE: 0, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  BLOOM_LIGHT: {
    FLAVOR: SPICY, 
    KIND: PROMO, 
    TRAIT: BLAND, 
    PRICE: 230, 
    RECIPE: {
      ADELHYDE: 4, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 2, 
      KARMOTRINE: 3, 
      ADD_ICE: True, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  BLUE_FAIRY: {
    FLAVOR: SWEET, 
    KIND: GIRLY, 
    TRAIT: SOFT, 
    PRICE: 170, 
    RECIPE: {
      ADELHYDE: 4, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 1, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  BRANDTINI: {
    FLAVOR: SWEET, 
    KIND: CLASSY, 
    TRAIT: HAPPY, 
    PRICE: 250, 
    RECIPE: {
      ADELHYDE: 6, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 3, 
      FLANERGIDE: 0, 
      KARMOTRINE: 1, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  COBALT_VELVET: {
    FLAVOR: BUBBLY, 
    KIND: CLASSY, 
    TRAIT: BURNING, 
    PRICE: 280, 
    RECIPE: {
      ADELHYDE: 2, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 3, 
      KARMOTRINE: 5, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  CREVICE_SPIKE: {
    FLAVOR: SOUR, 
    KIND: MANLY, 
    TRAIT: SOBERING, 
    PRICE: 140, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 2, 
      FLANERGIDE: 4, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  FLUFFY_DREAM: {
    FLAVOR: SOUR, 
    KIND: GIRLY, 
    TRAIT: SOFT, 
    PRICE: 170, 
    RECIPE: {
      ADELHYDE: 3, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 3, 
      FLANERGIDE: 0, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  FRINGE_WEAVER: {
    FLAVOR: BUBBLY, 
    KIND: CLASSY, 
    TRAIT: STRONG, 
    PRICE: 260, 
    RECIPE: {
      ADELHYDE: 1, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 0, 
      KARMOTRINE: 9, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  FROTHY_WATER: {
    FLAVOR: BUBBLY, 
    KIND: CLASSIC, 
    TRAIT: BLAND, 
    PRICE: 150, 
    RECIPE: {
      ADELHYDE: 1, 
      BRONSON_EXTRACT: 1, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 1, 
      KARMOTRINE: 0, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  GRIZZLY_TEMPLE: {
    FLAVOR: BITTER, 
    KIND: PROMO, 
    TRAIT: BLAND, 
    PRICE: 220, 
    RECIPE: {
      ADELHYDE: 3, 
      BRONSON_EXTRACT: 3, 
      POWDERED_DELTA: 3, 
      FLANERGIDE: 0, 
      KARMOTRINE: 1, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  GUT_PUNCH: {
    FLAVOR: BITTER, 
    KIND: MANLY, 
    TRAIT: STRONG, 
    PRICE: 80, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 5, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 1, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  MARSBLAST: {
    FLAVOR: SPICY, 
    KIND: MANLY, 
    TRAIT: STRONG, 
    PRICE: 170, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 6, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 4, 
      KARMOTRINE: 2, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  MERCURYBLAST: {
    FLAVOR: SOUR, 
    KIND: CLASSY, 
    TRAIT: BURNING, 
    PRICE: 250, 
    RECIPE: {
      ADELHYDE: 1, 
      BRONSON_EXTRACT: 1, 
      POWDERED_DELTA: 3, 
      FLANERGIDE: 3, 
      KARMOTRINE: 2, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  MOONBLAST: {
    FLAVOR: SWEET, 
    KIND: GIRLY, 
    TRAIT: HAPPY, 
    PRICE: 180, 
    RECIPE: {
      ADELHYDE: 6, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 1, 
      KARMOTRINE: 2, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  PIANO_MAN: {
    FLAVOR: SOUR, 
    KIND: PROMO, 
    TRAIT: STRONG, 
    PRICE: 320, 
    RECIPE: {
      ADELHYDE: 2, 
      BRONSON_EXTRACT: 3, 
      POWDERED_DELTA: 5, 
      FLANERGIDE: 5, 
      KARMOTRINE: 3, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  PIANO_WOMAN: {
    FLAVOR: SWEET, 
    KIND: PROMO, 
    TRAIT: HAPPY, 
    PRICE: 320, 
    RECIPE: {
      ADELHYDE: 5, 
      BRONSON_EXTRACT: 5, 
      POWDERED_DELTA: 2, 
      FLANERGIDE: 3, 
      KARMOTRINE: 3, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  PILEDRIVER: {
    FLAVOR: BITTER, 
    KIND: MANLY, 
    TRAIT: BURNING, 
    PRICE: 160, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 3, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 3, 
      KARMOTRINE: 4, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  SPARKLE_STAR: {
    FLAVOR: SWEET, 
    KIND: GIRLY, 
    TRAIT: HAPPY, 
    PRICE: 150, 
    RECIPE: {
      ADELHYDE: 2, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 0, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: True, 
      WAIT: False, 
    }, 
  },
  SUGAR_RUSH: {
    FLAVOR: SWEET, 
    KIND: GIRLY, 
    TRAIT: HAPPY, 
    PRICE: 150, 
    RECIPE: {
      ADELHYDE: 2, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 1, 
      FLANERGIDE: 0, 
      KARMOTRINE: -1, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  SUNSHINE_CLOUD: {
    FLAVOR: BITTER, 
    KIND: GIRLY, 
    TRAIT: SOFT, 
    PRICE: 150, 
    RECIPE: {
      ADELHYDE: 2, 
      BRONSON_EXTRACT: 2, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 0, 
      KARMOTRINE: -1, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: True, 
    }, 
  },
  SUPLEX: {
    FLAVOR: BITTER, 
    KIND: MANLY, 
    TRAIT: BURNING, 
    PRICE: 160, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 4, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 3, 
      KARMOTRINE: 3, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  ZEN_STAR: {
    FLAVOR: SOUR, 
    KIND: PROMO, 
    TRAIT: BLAND, 
    PRICE: 210, 
    RECIPE: {
      ADELHYDE: 4, 
      BRONSON_EXTRACT: 4, 
      POWDERED_DELTA: 4, 
      FLANERGIDE: 4, 
      KARMOTRINE: 4, 
      ADD_ICE: True, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  A_FEDORA_WITH_PERFUME_AND_A_PLUM: {
    FLAVOR: NONE, 
    KIND: NONE, 
    TRAIT: NONE, 
    PRICE: 500, 
    RECIPE: {
      ADELHYDE: 0, 
      BRONSON_EXTRACT: 0, 
      POWDERED_DELTA: 0, 
      FLANERGIDE: 0, 
      KARMOTRINE: 0, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
  FLAMING_MOAI: {
    FLAVOR: SOUR, 
    KIND: CLASSIC, 
    TRAIT: CLASSY, 
    PRICE: 150, 
    RECIPE: {
      ADELHYDE: 1, 
      BRONSON_EXTRACT: 1, 
      POWDERED_DELTA: 2, 
      FLANERGIDE: 3, 
      KARMOTRINE: 5, 
      ADD_ICE: False, 
      AGE: False, 
      WAIT: False, 
    }, 
  },
}