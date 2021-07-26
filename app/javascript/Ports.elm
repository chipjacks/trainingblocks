port module Ports exposing (handleScroll, scrollToSelectedDate, selectDateFromScroll, setDropTarget, setPointerCapture)

import Json.Encode


port scrollToSelectedDate : () -> Cmd msg


port selectDateFromScroll : (String -> msg) -> Sub msg


port setPointerCapture : { targetId : String, pointerId : Int } -> Cmd msg


port setDropTarget : (Int -> msg) -> Sub msg


port handleScroll : (Json.Encode.Value -> msg) -> Sub msg
