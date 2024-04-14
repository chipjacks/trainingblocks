module Settings exposing (Settings, decoder, encoder)

import Activity exposing (raceDistance)
import Activity.Types exposing (RaceDistance)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Pace.List exposing (PaceList)


type alias Settings =
    { paces : PaceList String
    , raceDistance : RaceDistance
    , raceDuration : Int
    , level : Int
    , showTime : Bool
    , stravaPost : Maybe Bool
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
        |> required "show_time" Decode.bool
        |> optional "strava_post" (Decode.map Just Decode.bool) Nothing


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
        , ( "show_time", Encode.bool settings.showTime )
        , ( "strava_post", maybeEncode settings.stravaPost Encode.bool )
        ]


-- UTIL


maybeEncode : Maybe a -> (a -> Encode.Value) -> Encode.Value
maybeEncode fieldM encoder_ =
    case fieldM of
        Just field ->
            encoder_ field

        Nothing ->
            Encode.null
