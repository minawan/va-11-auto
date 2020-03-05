include "shared.thrift"
include "recipe.thrift"

struct ResetAction { }

struct SelectSlotAction {
    1: shared.ScreenElementType slot;
}

struct AddIngredientAction {
    1: shared.ScreenElementType ingredient;
    2: i32 quantity;
}

struct MixAction {
    1: i32 durationInSeconds;
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
    1: recipe.DrinkRecipe drinkRecipe;
    2: bool reset;
    3: shared.ScreenElementType slot;
    4: bool serve;
}

struct RecipeActionResponse {
    1: list<RecipeAction> actions;
}

service RecipeActionService {
    RecipeActionResponse getRecipeActions(1:RecipeActionRequest request)
}
