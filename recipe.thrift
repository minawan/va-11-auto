enum DrinkName {
    BEER
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
