module Pace exposing (StandardPace(..), calculate, paceFromString, paceToString, secondsToStandardPace, standardPace, standardPaceToSeconds, standardPaces)

import Activity.Types exposing (ActivityData, DistanceUnits(..))
import Array exposing (Array)
import Distance
import Duration
import Enum exposing (Enum)
import Json.Decode as Decode
import MPRData
import MPRLevel exposing (RunnerType(..))
import Pace.List exposing (PaceList)
import Parser


calculate : Int -> Float -> Int
calculate duration distance =
    toFloat duration
        / Distance.fromMeters Miles distance
        |> round


paceToString : Int -> String
paceToString seconds =
    Duration.toString seconds


paceFromString : String -> Maybe Int
paceFromString str =
    case Parser.run Duration.parser str of
        Ok [ 0, mins, secs ] ->
            Just (mins * 60 + secs)

        Ok [ mins, secs ] ->
            Just (mins * 60 + secs)

        Ok [ mins ] ->
            Just (mins * 60)

        _ ->
            Nothing



-- TRAINING PACES


standardPacesTable : RunnerType -> Array (Array ( String, String ))
standardPacesTable runnerType =
    let
        json =
            case runnerType of
                MPRLevel.Neutral ->
                    MPRData.neutralTraining

                MPRLevel.Aerobic ->
                    MPRData.aerobicTraining

                MPRLevel.Speed ->
                    MPRData.speedTraining
    in
    Decode.decodeString (Decode.array (Decode.array (Decode.list Decode.string))) json
        |> Result.withDefault Array.empty
        |> Array.map (\a -> Array.map (\t -> toTuple t |> Maybe.withDefault ( "", "" )) a)


standardPaces : ( RunnerType, Int ) -> PaceList StandardPace
standardPaces ( runnerType, level ) =
    Array.get (level - 1) (standardPacesTable runnerType)
        |> Maybe.map
            (\arr ->
                Array.toList arr
                    |> List.map2 Tuple.pair (List.map Tuple.second (List.drop 1 standardPace.list))
                    |> List.map (\( pace, ( min, max ) ) -> ( pace, paceFromString max |> Maybe.withDefault 0 ))
            )
        |> Maybe.withDefault []


standardPaceToSeconds : PaceList StandardPace -> StandardPace -> Int
standardPaceToSeconds paces tp =
    if tp == VeryEasy then
        List.head paces
            |> Maybe.map Tuple.second
            |> Maybe.withDefault 0

    else
        Pace.List.lookupSeconds tp paces
            |> Maybe.withDefault 0


secondsToStandardPace : PaceList StandardPace -> Int -> StandardPace
secondsToStandardPace paces seconds =
    Pace.List.lookupValue seconds paces
        |> Maybe.withDefault VeryEasy


type StandardPace
    = VeryEasy
    | Easy
    | Moderate
    | Steady
    | Brisk
    | Aerobic
    | Lactate
    | Groove
    | VO2
    | Fast


standardPace : Enum StandardPace
standardPace =
    Enum.create
        [ ( "Very Easy", VeryEasy )
        , ( "Easy", Easy )
        , ( "Moderate", Moderate )
        , ( "Steady", Steady )
        , ( "Brisk", Brisk )
        , ( "Aerobic", Aerobic )
        , ( "Lactate", Lactate )
        , ( "Groove", Groove )
        , ( "VO2", VO2 )
        , ( "Fast", Fast )
        ]


toTuple : List a -> Maybe ( a, a )
toTuple l =
    case l of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing
