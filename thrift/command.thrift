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

service CommandService {
  list<Command> getCommands()
}
