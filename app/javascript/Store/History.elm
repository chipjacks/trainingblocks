module Store.History exposing (History, events, init, isEmpty, length, push)


type History event state
    = History (List ( event, state ))


init : History event state
init =
    History []


isEmpty : History event state -> Bool
isEmpty (History list) =
    List.isEmpty list


length : History event state -> Int
length (History list) =
    List.length list


events : History event state -> List event
events (History list) =
    List.unzip list
        |> Tuple.first


push : ( event, state ) -> History event state -> History event state
push ( event, state ) (History list) =
    History (( event, state ) :: list)
