port module Ports exposing (handleScroll, scrollCompleted, selectDateFromScroll, setDropTarget, setPointerCapture)

import Json.Encode


port selectDateFromScroll : (String -> msg) -> Sub msg


port setPointerCapture : { targetId : String, pointerId : Int } -> Cmd msg


port setDropTarget : (Int -> msg) -> Sub msg


port handleScroll : (Json.Encode.Value -> msg) -> Sub msg


port scrollCompleted : (Bool -> msg) -> Sub msg
