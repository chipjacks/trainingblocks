module Activity.Data exposing (Filter, completed, effort, list, other, planned, race, run, visible)

import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), Effort(..), LapData(..))


type alias Filter =
    Activity -> ActivityData -> Bool


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
        |> List.filter (\data -> List.all (\f -> f activity data) filters)



-- FILTERS


visible : Filter
visible =
    \activity data ->
        case ( List.isEmpty activity.laps, List.isEmpty activity.planned ) of
            ( False, _ ) ->
                completed activity data

            ( _, False ) ->
                planned activity data

            ( True, True ) ->
                completed activity data


planned : Filter
planned =
    \_ data -> data.completed == Planned


completed : Filter
completed =
    \_ data -> data.completed == Completed


run : Filter
run =
    \_ data -> data.activityType == Run


other : Filter
other =
    \_ data -> data.activityType == Other


effort : Maybe Effort -> Filter
effort e =
    \_ data -> data.effort == e


race : Filter
race =
    \_ data -> data.race /= Nothing
