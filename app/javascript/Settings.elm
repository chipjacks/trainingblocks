module Settings exposing (Settings, decoder, encoder)

import Activity exposing (raceDistance)
import Activity.Types exposing (RaceDistance)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Pace.List exposing (PaceList)


type alias Settings =
    { paces : PaceList String
    , raceDistance : RaceDistance
    , raceDuration : Int
    , level : Int
    }


decoder : Decode.Decoder Settings
decoder =
    let
        paceDecoder =
            Decode.map2 Tuple.pair (Decode.field "name" Decode.string) (Decode.field "pace" Decode.int)
    in
    Decode.succeed Settings
        |> required "paces" (Decode.list paceDecoder)
        |> required "race_distance" raceDistance.decoder
        |> required "race_duration" Decode.int
        |> required "level" Decode.int


encoder : Settings -> Encode.Value
encoder settings =
    let
        pacesEncoder ( name, pace ) =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "pace", Encode.int pace )
                ]
    in
    Encode.object
        [ ( "paces", Encode.list pacesEncoder settings.paces )
        , ( "race_distance", raceDistance.encode settings.raceDistance )
        , ( "race_duration", Encode.int settings.raceDuration )
        , ( "level", Encode.int settings.level )
        ]
