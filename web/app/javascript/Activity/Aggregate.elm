module Activity.Aggregate exposing (distance, duration)

import Activity.Data exposing (Filter)
import Activity.Types exposing (Activity, ActivityData)


duration : List Filter -> List Activity -> Int
duration filters activities =
    List.concatMap (Activity.Data.list filters) activities
        |> List.map (\data -> data.duration |> Maybe.withDefault 0)
        |> List.sum


distance : List Filter -> List Activity -> Float
distance filters activities =
    List.concatMap (Activity.Data.list filters) activities
        |> List.map (\data -> data.distance |> Maybe.withDefault 0)
        |> List.sum


elevationGain : List Filter -> List Activity -> Float
elevationGain filters activities =
    List.concatMap (Activity.Data.list filters) activities
        |> List.map (\data -> data.elevationGain |> Maybe.withDefault 0)
        |> List.sum
