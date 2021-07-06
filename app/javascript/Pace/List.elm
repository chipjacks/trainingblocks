module Pace.List exposing (PaceList, lookupSeconds, lookupValue)


type alias PaceList a =
    List ( a, Int )


lookupSeconds : a -> PaceList a -> Maybe Int
lookupSeconds value paces =
    List.filter (\( name, _ ) -> name == value) paces
        |> List.head
        |> Maybe.map Tuple.second


lookupValue : Int -> PaceList a -> Maybe a
lookupValue seconds paces =
    List.filter (\( name, maxPaceSeconds ) -> seconds <= maxPaceSeconds) paces
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first
