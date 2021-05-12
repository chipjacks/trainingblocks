module Store.History exposing (History, append, events, init, isEmpty, length, peek, pop, push, version)


type History event state
    = History (List ( event, state )) Int


init : History event state
init =
    History [] 0


isEmpty : History event state -> Bool
isEmpty (History list _) =
    List.isEmpty list


version : History event state -> Int
version (History list v) =
    v


length : History event state -> Int
length (History list v) =
    List.length list


events : History event state -> List event
events (History list _) =
    List.unzip list
        |> Tuple.first


push : ( event, state ) -> History event state -> History event state
push ( event, state ) (History list v) =
    History (( event, state ) :: list) (v + 1)


append : History event state -> History event state -> History event state
append (History list v) (History more _) =
    History (list ++ more) (v + 1)


peek : History event state -> Maybe ( event, state )
peek (History list _) =
    List.head list


pop : History event state -> ( Maybe ( event, state ), History event state )
pop (History list v) =
    case list of
        head :: tail ->
            ( Just head, History tail (v + 1) )

        [] ->
            ( Nothing, History list (v + 1) )
