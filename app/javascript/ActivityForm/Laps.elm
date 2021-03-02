module ActivityForm.Laps exposing (Laps, add, copy, delete, get, init, select, set, shift, updateAll)

import Activity
import Activity.Types exposing (Activity, ActivityData, LapData(..))
import Array


type alias Laps =
    ( Int, List LapData )


init : List LapData -> Laps
init laps =
    ( 0, laps )


select : Int -> Laps -> Laps
select newIndex ( index, laps ) =
    ( newIndex, laps )


get : Laps -> ActivityData
get ( index, laps ) =
    Array.fromList laps
        |> Array.get index
        |> Maybe.map
            (\lap ->
                case lap of
                    Individual data ->
                        data

                    _ ->
                        Activity.initActivityData
            )
        |> Maybe.withDefault Activity.initActivityData


set : ActivityData -> Laps -> Laps
set data ( index, laps ) =
    ( index
    , Array.fromList laps
        |> Array.set index (Individual data)
        |> Array.toList
    )


updateAll : (ActivityData -> ActivityData) -> Laps -> Laps
updateAll transform ( index, laps ) =
    let
        nestedTransform lap =
            case lap of
                Individual data ->
                    Individual (transform data)

                Repeats count list ->
                    Repeats count (List.map transform list)
    in
    ( index
    , List.map nestedTransform laps
    )


add : ActivityData -> Laps -> Laps
add lap ( index, laps ) =
    ( List.length laps, laps ++ [ Individual lap ] )


copy : Laps -> Laps
copy ( index, laps ) =
    let
        tail =
            List.drop index laps

        copied =
            case List.head tail of
                Just lap ->
                    [ lap ]

                _ ->
                    []
    in
    ( index + 1
    , List.take index laps ++ copied ++ tail
    )


shift : Bool -> Laps -> Laps
shift up ( index, laps ) =
    let
        ( indexA, indexB ) =
            if up then
                ( index - 1, index )

            else
                ( index, index + 1 )

        lapsArray =
            Array.fromList laps

        shiftedLaps =
            [ Array.slice 0 indexA lapsArray
            , Array.slice indexB (indexB + 1) lapsArray
            , Array.slice indexA (indexA + 1) lapsArray
            , Array.slice (indexB + 1) (Array.length lapsArray) lapsArray
            ]
                |> List.map Array.toList
                |> List.concat
    in
    if indexA < 0 || indexB >= List.length laps then
        ( index
        , laps
        )

    else
        ( if up then
            index - 1

          else
            index + 1
        , shiftedLaps
        )


delete : Laps -> Laps
delete ( index, laps ) =
    ( if index < (List.length laps - 1) then
        index

      else
        index - 1
    , List.take index laps ++ List.drop (index + 1) laps
    )
