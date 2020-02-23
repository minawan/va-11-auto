include "recipe.thrift"

enum ScreenElementType {
    RESET
    LEFT_SLOT
    RIGHT_SLOT
    ADELHYDE
    BRONSON_EXTRACT
    POWDERED_DELTA
    FLANERGIDE
    KARMOTRINE
    BLENDER
    ADD_ICE
    AGE
    MIX
}

struct ResetAction { }

struct SelectSlotAction {
    1: optional ScreenElementType slot;
}

struct AddIngredientAction {
    1: optional ScreenElementType ingredient;
    2: optional i32 amount;
}

struct MixAction {
    1: optional i32 durationInSeconds;
}

struct AddIceAction { }

struct AgeAction { }

struct ServeAction { }

union RecipeAction {
    1: ResetAction resetAction;
    2: SelectSlotAction selectSlotAction;
    3: AddIngredientAction addIngredientAction;
    4: AddIceAction addIceAction;
    5: AgeAction ageAction;
    6: MixAction mixAction;
    7: ServeAction serveAction;
}

struct RecipeActionRequest {
    1: optional recipe.DrinkRecipe drinkRecipe;
    2: optional bool reset;
    3: optional ScreenElementType slot;
    4: optional bool serve;
}

struct RecipeActionResponse {
    1: list<RecipeAction> actions;
}

service RecipeActionServer {
    RecipeActionResponse getRecipeActions(1:RecipeActionRequest request)
}
