port module Ports exposing (scrollToSelectedDate, selectDateFromScroll, setPointerCapture)


port scrollToSelectedDate : () -> Cmd msg


port selectDateFromScroll : (String -> msg) -> Sub msg


port setPointerCapture : { targetId : String, pointerId : Int } -> Cmd msg
