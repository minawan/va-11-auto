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
    1: optional DrinkName drinkName;
    2: optional bool addKarmotrine;
    3: optional bool bigSize;
}

struct DrinkRecipe {
    1: optional i32 adelhyde;
    2: optional i32 bronsonExtract;
    3: optional i32 powderedDelta;
    4: optional i32 flanergide;
    5: optional i32 karmotrine;
    6: optional bool addIce;
    7: optional bool age;
    8: optional bool blend;
}

struct DrinkRecipeResponse {
    1: optional DrinkRecipe drinkRecipe;
}

service DrinkRecipeServer {
    DrinkRecipeResponse getDrinkRecipe(1:DrinkRecipeRequest request)
}
