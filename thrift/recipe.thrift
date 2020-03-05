include "shared.thrift"

enum DrinkName {
    BAD_TOUCH
    BEER
    BLEEDING_JANE
    BLOOM_LIGHT
    BLUE_FAIRY
    BRANDTINI
    COBALT_VELVET
    CREVICE_SPIKE
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
    FLAMING_MOAI
}

struct DrinkRecipeRequest {
    1: DrinkName drinkName;
    2: bool addKarmotrine;
    3: bool bigSize;
}

struct DrinkRecipe {
    1: map<shared.ScreenElementType, i32> quantity;
    2: bool addIce;
    3: bool age;
    4: bool blend;
}

struct DrinkRecipeResponse {
    1: DrinkRecipe drinkRecipe;
}

service DrinkRecipeService {
    DrinkRecipeResponse getDrinkRecipe(1:DrinkRecipeRequest request)
}
