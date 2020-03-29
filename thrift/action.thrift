include "shared.thrift"
include "recipe.thrift"

struct ResetAction {}

struct SelectSlotAction {
  1: shared.ScreenElementType name;
}

struct AddIngredientAction {
  1: shared.ScreenElementType name;
  2: i32 quantity;
}

struct MixAction {
  1: shared.ScreenElementType name;
  2: i32 durationInSeconds;
}

struct AddIceAction {
  1: shared.ScreenElementType name;
}

struct AgeAction {
  1: shared.ScreenElementType name;
}

struct ServeAction {
  1: shared.ScreenElementType name;
}

union RecipeAction {
  1: ResetAction resetAction;
  2: SelectSlotAction selectSlotAction;
  3: AddIngredientAction addIngredientAction;
  4: AddIceAction addIceAction;
  5: AgeAction ageAction;
  6: MixAction mixAction;
  7: ServeAction serveAction;
}

service RecipeActionService {
  i32 getRecipeActions(
    1: i32 transactionId,
    2: bool reset,
    3: shared.ScreenElementType slot,
    4: bool serve)
}
