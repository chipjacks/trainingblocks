port module Ports exposing (scrollToSelectedDate, selectDateFromScroll)


port scrollToSelectedDate : () -> Cmd msg


port selectDateFromScroll : (String -> msg) -> Sub msg
