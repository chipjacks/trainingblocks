module Activity.Laps exposing (listData, set, sum)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), LapData(..))


listData : Activity -> List ActivityData
listData activity =
    activity.laps
        |> Maybe.withDefault [ Individual activity.data ]
        |> List.concatMap
            (\lap ->
                case lap of
                    Individual data ->
                        List.singleton data

                    Repeats _ list ->
                        []
            )


set : Activity -> List LapData -> Activity
set activity laps =
    case laps of
        [] ->
            { activity | laps = Nothing }

        list ->
            { activity | data = sum list, laps = Just list }


sum : List LapData -> ActivityData
sum laps =
    ActivityData
        Run
        (Just (duration laps))
        (completed laps)
        Nothing
        Nothing
        Nothing
        Nothing


duration : List LapData -> Int
duration laps =
    laps
        |> List.map
            (\lap ->
                case lap of
                    Individual data ->
                        .duration data |> Maybe.withDefault 0

                    Repeats _ list ->
                        List.filterMap .duration list |> List.sum
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
