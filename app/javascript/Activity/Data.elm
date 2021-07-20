module Activity.Data exposing (Filter, completed, list, other, planned, race, run, visible)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), LapData(..))


type alias Filter =
    ActivityData -> Bool


list : List Filter -> Activity -> List ActivityData
list filters activity =
    (activity.laps ++ activity.planned)
        |> List.concatMap
            (\lap ->
                case lap of
                    Individual data ->
                        List.singleton data

                    Repeats count datas ->
                        List.concat (List.repeat count datas)
            )
        |> List.filter (\data -> List.all (\f -> f data) filters)



-- FILTERS


visible : Activity -> Filter
visible activity =
    case ( List.isEmpty activity.laps, List.isEmpty activity.planned ) of
        ( False, _ ) ->
            completed

        ( _, False ) ->
            planned

        ( True, True ) ->
            completed


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


race : Filter
race =
    \data -> data.race /= Nothing
