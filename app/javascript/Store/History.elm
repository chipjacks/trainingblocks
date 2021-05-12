module Store.History exposing (History, append, events, init, isEmpty, length, peek, pop, push)


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


append : History event state -> History event state -> History event state
append (History list) (History more) =
    History (list ++ more)


peek : History event state -> Maybe ( event, state )
peek (History list) =
    List.head list


pop : History event state -> ( Maybe ( event, state ), History event state )
pop (History list) =
    case list of
        head :: tail ->
            ( Just head, History tail )

        [] ->
            ( Nothing, History list )
