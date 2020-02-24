include "recipe.thrift"
include "shared.thrift"

struct Coord {
    1: i32 x;
    2: i32 y;
}

struct ClickCommand {
    1: Coord position;
}

struct DragAndDropCommand {
    1: Coord source;
    2: Coord destination;
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

service CommandServer {
    CommandResponse getCommands(1:CommandRequest request)
}
