module Activity.Laps exposing (duration, listData, sum, updateField, visible)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), LapData(..))


visible : Activity -> List LapData
visible activity =
    case ( List.isEmpty activity.laps, List.isEmpty activity.planned ) of
        ( False, _ ) ->
            activity.laps

        ( _, False ) ->
            activity.planned

        ( True, True ) ->
            []


listData : Activity -> List ActivityData
listData activity =
    visible activity
        |> List.concatMap
            (\lap ->
                case lap of
                    Individual data ->
                        List.singleton data

                    Repeats count list ->
                        List.concat (List.repeat count list)
            )


updateField : (ActivityData -> ActivityData) -> LapData -> LapData
updateField transform lap =
    case lap of
        Individual data ->
            Individual (transform data)

        Repeats count list ->
            Repeats count (List.map transform list)


sum : List LapData -> ActivityData
sum laps =
    case laps of
        [ Individual data ] ->
            data

        _ ->
            { activityType = Run
            , duration = Just (duration laps)
            , completed = completed laps
            , pace = Nothing
            , distance = Nothing
            , distanceUnits = Nothing
            , race = Nothing
            , effort = Nothing
            , emoji = Nothing
            }


duration : List LapData -> Int
duration laps =
    laps
        |> List.map
            (\lap ->
                case lap of
                    Individual data ->
                        .duration data |> Maybe.withDefault 0

                    Repeats count list ->
                        (List.filterMap .duration list |> List.sum) * count
            )
        |> List.sum


completed : List LapData -> Completion
completed laps =
    let
        aggregator list =
            if List.all (\c -> c == Completed) list then
                Completed

            else
                Planned
    in
    laps
        |> List.map
            (\lap ->
                case lap of
                    Individual data ->
                        .completed data

                    Repeats _ list ->
                        aggregator (List.map .completed list)
            )
        |> aggregator
