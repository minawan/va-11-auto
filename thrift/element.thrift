include "shared.thrift"

struct ScreenElementRequest {
    1: shared.ScreenElementType screenElementName;
}

struct ScreenElementResponse {
    1: shared.ScreenElement screenElement;
}

service ScreenElementService {
    ScreenElementResponse getScreenElement(1:ScreenElementRequest request)
}
