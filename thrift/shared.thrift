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

struct Coord {
    1: i32 x;
    2: i32 y;
}

struct ScreenElement {
    1: Coord centroid;
    2: i32 shortcut;
}
