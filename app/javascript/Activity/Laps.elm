module Activity.Laps exposing (sum, updateField)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), LapData(..))


updateField : (ActivityData -> ActivityData) -> LapData -> LapData
updateField transform lap =
    case lap of
        Individual data ->
            Individual (transform data)

        Repeats count list ->
            Repeats count (List.map transform list)


sum : List ActivityData -> ActivityData
sum laps =
    case laps of
        [ data ] ->
            data

        _ ->
            { activityType = Run
            , duration = Just (duration laps)
            , completed = completed laps
            , pace = Nothing
            , distance = Nothing
            , distanceUnits = Nothing
            , elevationGain = Just (elevationGain laps)
            , race = Nothing
            , effort = Nothing
            , emoji = Nothing
            }


duration : List ActivityData -> Int
duration list =
    List.filterMap .duration list
        |> List.sum


elevationGain : List ActivityData -> Float
elevationGain list =
    List.filterMap .elevationGain list
        |> List.sum


completed : List ActivityData -> Completion
completed list =
    if List.all (\d -> d.completed == Completed) list then
        Completed

    else
        Planned
