include "shared.thrift"

enum DrinkName {
  A_FEDORA_WITH_PERFUME_AND_A_PLUM
  BAD_TOUCH
  BEER
  BLEEDING_JANE
  BLOOM_LIGHT
  BLUE_FAIRY
  BRANDTINI
  COBALT_VELVET
  CREVICE_SPIKE
  FLAMING_MOAI
  FLUFFY_DREAM
  FRINGE_WEAVER
  FROTHY_WATER
  GRIZZLY_TEMPLE
  GUT_PUNCH
  MARSBLAST
  MERCURYBLAST
  MOONBLAST
  PIANO_MAN
  PIANO_WOMAN
  PILEDRIVER
  SPARKLE_STAR
  SUGAR_RUSH
  SUNSHINE_CLOUD
  SUPLEX
  ZEN_STAR
}

struct DrinkRecipe {
  1: map<shared.ScreenElementType, i32> quantity;
  2: bool addIce;
  3: bool age;
  4: bool blend;
}

service DrinkRecipeService {
  i32 getDrinkRecipe(
    1: DrinkName drinkName,
	2: bool addKarmotrine,
	3: bool bigSize)
}
