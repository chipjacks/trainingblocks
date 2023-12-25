module MPRLevel exposing (RunnerType(..), distanceList, equivalentRaceTimes, lookup)

import Array exposing (Array)
import Dict exposing (Dict)
import Duration
import Json.Decode exposing (array, decodeString, dict, string)
import MPRData
import Result



-- Load tables


distanceList : List String
distanceList =
    [ "5k", "8k", "5 mile", "10k", "15k", "10 mile", "20k", "Half Marathon", "25k", "30k", "Marathon" ]


equivalentRaceTimesTable : RunnerType -> Dict String (Array String)
equivalentRaceTimesTable runnerType =
    let
        json =
            case runnerType of
                Neutral ->
                    MPRData.neutralRace

                Aerobic ->
                    MPRData.aerobicRace

                Speed ->
                    MPRData.speedRace
    in
    decodeString (dict (array string)) json |> Result.withDefault Dict.empty



-- Access Tables


type RunnerType
    = Neutral
    | Aerobic
    | Speed


lookup : RunnerType -> String -> Int -> Result String ( RunnerType, Int )
lookup runnerType distance seconds =
    Dict.get distance (equivalentRaceTimesTable runnerType)
        |> Result.fromMaybe ("Invalid distance: " ++ distance)
        |> Result.andThen (Array.map Duration.timeStrToSeconds >> Ok)
        |> Result.andThen (Array.foldr (Result.map2 (::)) (Ok []))
        |> Result.andThen (List.filter (\n -> n > seconds) >> Ok)
        |> Result.andThen (List.length >> Ok)
        |> Result.andThen
            (\l ->
                if l == 61 then
                    Err "That time is too fast!"

                else if l == 0 then
                    Err "That time is too slow!"

                else
                    Ok ( runnerType, l )
            )


equivalentRaceTimes : ( RunnerType, Int ) -> Result String (List ( String, String ))
equivalentRaceTimes ( runnerType, level ) =
    distanceList
        |> List.map (\d -> ( d, Dict.get d (equivalentRaceTimesTable runnerType) |> Maybe.withDefault Array.empty ))
        |> List.map (\( k, v ) -> ( k, Array.get level v |> Result.fromMaybe "out of range" ))
        |> List.foldr
            (\( k, v ) b ->
                b
                    |> Result.andThen
                        (\l ->
                            v
                                |> Result.andThen
                                    (\i ->
                                        Ok (( k, i ) :: l)
                                    )
                        )
            )
            (Ok [])



-- Utility functions





