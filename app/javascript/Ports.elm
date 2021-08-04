port module Ports exposing (handleScroll, selectDateFromScroll, setDropTarget, setPointerCapture)

import Json.Encode


port selectDateFromScroll : (String -> msg) -> Sub msg


port setPointerCapture : { targetId : String, pointerId : Int } -> Cmd msg


port setDropTarget : (Int -> msg) -> Sub msg


port handleScroll : (Bool -> msg) -> Sub msg
