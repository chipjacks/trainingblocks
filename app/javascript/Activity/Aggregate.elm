module Activity.Aggregate exposing (completed, distance, duration, other, planned, run)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), LapData(..))



{-
   - {planned, completed} {run, other, total} { duration, distance }
-}


type alias Filter =
    ActivityData -> Bool


duration : List Filter -> List Activity -> Int
duration filters activities =
    listData activities
        |> List.filter (\data -> List.all (\f -> f data) filters)
        |> List.map (\data -> data.duration |> Maybe.withDefault 0)
        |> List.sum


distance : List Filter -> List Activity -> Float
distance filters activities =
    listData activities
        |> List.filter (\data -> List.all (\f -> f data) filters)
        |> List.map (\data -> data.distance |> Maybe.withDefault 0)
        |> List.sum



-- LIST


listData : List Activity -> List ActivityData
listData activities =
    List.concatMap (\a -> a.laps ++ a.planned) activities
        |> List.concatMap
            (\lap ->
                case lap of
                    Individual data ->
                        List.singleton data

                    Repeats count list ->
                        List.concat (List.repeat count list)
            )



-- FILTER


planned : Filter
planned =
    \data -> data.completed == Planned


completed : Filter
completed =
    \data -> data.completed == Completed


run : Filter
run =
    \data -> data.activityType == Run


other : Filter
other =
    \data -> data.activityType == Other
