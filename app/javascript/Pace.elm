module Pace exposing (Pace, TrainingPace(..), paceFromString, paceToString, secondsToTrainingPace, trainingPace, trainingPaceToSeconds, trainingPaces)

import Array exposing (Array)
import Duration
import Enum exposing (Enum)
import Json.Decode as Decode
import MPRData
import MPRLevel exposing (RunnerType(..))
import Parser


type alias Pace =
    Int


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


trainingPacesTable : RunnerType -> Array (Array ( String, String ))
trainingPacesTable runnerType =
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


trainingPaces : ( RunnerType, Int ) -> Result String (List ( TrainingPace, ( String, String ) ))
trainingPaces ( runnerType, level ) =
    let
        res =
            Array.get (level - 1) (trainingPacesTable runnerType)
    in
    case res of
        Just arr ->
            Ok (Array.toList arr |> List.map2 (\x y -> Tuple.pair x y) (List.map Tuple.second (List.drop 1 trainingPace.list)))

        Nothing ->
            Err "out of range"


trainingPaceToSeconds : Int -> TrainingPace -> Int
trainingPaceToSeconds level tp =
    trainingPaces ( MPRLevel.Neutral, level )
        |> Result.map
            (\l ->
                List.filter (\( name, _ ) -> name == tp) l
                    |> List.head
                    |> Maybe.map
                        (\( _, ( minPace, maxPace ) ) -> paceFromString maxPace |> Maybe.withDefault 0)
                    |> Maybe.withDefault 0
            )
        |> Result.withDefault 0


secondsToTrainingPace : Int -> Int -> TrainingPace
secondsToTrainingPace level seconds =
    trainingPaces ( MPRLevel.Neutral, level )
        |> Result.toMaybe
        |> Maybe.andThen
            (\list ->
                List.map (\( name, ( minPace, maxPace ) ) -> ( name, Duration.timeStrToSeconds maxPace |> Result.withDefault 0 )) list
                    |> List.filter (\( name, maxPaceSeconds ) -> seconds <= maxPaceSeconds)
                    |> List.reverse
                    |> List.head
                    |> Maybe.map Tuple.first
            )
        |> Maybe.withDefault Slow


type TrainingPace
    = Slow
    | Easy
    | Moderate
    | Steady
    | Brisk
    | Aerobic
    | Lactate
    | Groove
    | VO2
    | Fast


trainingPace : Enum TrainingPace
trainingPace =
    Enum.create
        [ Slow
        , Easy
        , Moderate
        , Steady
        , Brisk
        , Aerobic
        , Lactate
        , Groove
        , VO2
        , Fast
        ]
        (\a ->
            case a of
                Slow ->
                    "Slow"

                Easy ->
                    "Easy"

                Moderate ->
                    "Moderate"

                Steady ->
                    "Steady"

                Brisk ->
                    "Brisk"

                Aerobic ->
                    "Aerobic"

                Lactate ->
                    "Lactate"

                Groove ->
                    "Groove"

                VO2 ->
                    "VO2"

                Fast ->
                    "Fast"
        )


toTuple : List a -> Maybe ( a, a )
toTuple l =
    case l of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing
