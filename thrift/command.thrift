include "action.thrift"
include "recipe.thrift"
include "shared.thrift"

struct ClickCommand {
    1: shared.Coord position;
}

struct DragAndDropCommand {
    1: shared.Coord source;
    2: shared.Coord destination;
}

struct TypeCommand {
    1: i8 key;
}

struct WaitCommand {
    1: i32 durationInSeconds;
}

union Command {
    1: ClickCommand clickCommand;
    2: DragAndDropCommand dragAndDropCommand;
    3: TypeCommand typeCommand;
    4: WaitCommand waitCommand;
}

struct CommandRequest {
    1: recipe.DrinkName drinkName;
    2: bool addKarmotrine;
    3: bool bigSize;
    4: bool reset;
    5: shared.ScreenElementType slot;
    6: bool serve;
    7: bool useShortcut;
}

struct CommandResponse {
    1: list<Command> commands;
}

service CommandService {
    CommandResponse getCommands(1:CommandRequest request)
    CommandResponse convertActionToCommands(1:map<shared.ScreenElementType, shared.ScreenElement> screenElements, 2:action.RecipeAction action, 3:bool useShortcut)
}
