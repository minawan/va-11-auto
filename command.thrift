include "recipe.thrift"
include "action.thrift"

struct CommandRequest {
    1: optional recipe.DrinkName drinkName;
    2: optional bool addKarmotrine;
    3: optional bool bigSize;
    4: optional bool reset;
    5: optional action.ScreenElementType slot;
    6: optional bool serve;
    7: optional bool useShortcut;
}
