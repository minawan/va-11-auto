include "shared.thrift"

service ScreenElementService {
    shared.ScreenElement getScreenElement(1:shared.ScreenElementType screenElementName)
}
