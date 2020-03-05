include "shared.thrift"

struct ScreenElementRequest {
    1: shared.ScreenElementType screenElementName;
}

struct ScreenElementResponse {
    1: shared.Coord centroid;
    2: i32 shortcut;
}

service ScreenElementService {
    ScreenElementResponse getScreenElement(1:ScreenElementRequest request)
}
