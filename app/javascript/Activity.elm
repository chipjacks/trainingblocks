module Activity exposing (activityType, decoder, distanceUnits, effort, encoder, initActivityData, mprLevel, newId, raceDistance)

import Activity.Data
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), DistanceUnits(..), Effort(..), LapData(..), RaceDistance(..))
import Date exposing (Date)
import Enum exposing (Enum)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Json.Encode as Encode
import MPRLevel
import Random


initActivityData : ActivityData
initActivityData =
    { activityType = Run
    , duration = Nothing
    , completed = Completed
    , pace = Nothing
    , distance = Nothing
    , distanceUnits = Nothing
    , race = Nothing
    , effort = Nothing
    , emoji = Nothing
    }


activityType : Enum ActivityType
activityType =
    Enum.create
        [ ( "Run", Run )
        , ( "Other", Other )
        ]


distanceUnits : Enum DistanceUnits
distanceUnits =
    Enum.create
        [ ( "mi", Miles )
        , ( "km", Kilometers )
        , ( "m", Meters )
        , ( "yd", Yards )
        ]


raceDistance : Enum RaceDistance
raceDistance =
    Enum.create
        [ ( "5k", FiveK )
        , ( "8k", EightK )
        , ( "5 mile", FiveMile )
        , ( "10k", TenK )
        , ( "15k", FifteenK )
        , ( "10 mile", TenMile )
        , ( "20k", TwentyK )
        , ( "Half Marathon", HalfMarathon )
        , ( "25k", TwentyFiveK )
        , ( "30k", ThirtyK )
        , ( "Marathon", Marathon )
        , ( "Other", OtherDistance )
        ]


effort : Enum Effort
effort =
    Enum.create
        [ ( "Easy", Easy )
        , ( "Moderate", Moderate )
        , ( "Hard", Hard )
        ]


newId : Random.Generator String
newId =
    let
        digitsToString digits =
            List.map String.fromInt digits
                |> String.join ""
    in
    Random.list 10 (Random.int 0 9)
        |> Random.map digitsToString


mprLevel : Activity -> Maybe ( Int, ActivityData )
mprLevel activity =
    let
        data =
            Activity.Data.list [ Activity.Data.race ] activity
                |> List.head
                |> Maybe.withDefault initActivityData
    in
    case ( data.race, data.duration ) of
        ( Just distance_, Just duration ) ->
            MPRLevel.lookup MPRLevel.Neutral
                (raceDistance.toString distance_)
                duration
                |> Result.map (\( _, level ) -> level)
                |> Result.toMaybe
                |> Maybe.map (\l -> ( l, data ))

        _ ->
            Nothing



-- SERIALIZATION


decoder : Decode.Decoder Activity
decoder =
    Decode.succeed Activity
        |> required "id" Decode.string
        |> required "date" dateDecoder
        |> required "description" Decode.string
        |> optionalAt [ "data", "laps" ] (Decode.list lapDataDecoder) []
        |> optionalAt [ "data", "planned" ] (Decode.list lapDataDecoder) []
        |> optional "import_id" (Decode.map Just Decode.string) Nothing


lapDataDecoder : Decode.Decoder LapData
lapDataDecoder =
    Decode.oneOf
        [ Decode.map Individual activityDataDecoder
        , Decode.map2 Repeats (Decode.field "repeats" Decode.int) (Decode.field "laps" (Decode.list activityDataDecoder))
        ]


activityDataDecoder : Decode.Decoder ActivityData
activityDataDecoder =
    let
        completedDecoder =
            Decode.bool
                |> Decode.andThen
                    (\c ->
                        if c then
                            Decode.succeed Completed

                        else
                            Decode.succeed Planned
                    )
    in
    Decode.succeed ActivityData
        |> required "type" activityType.decoder
        |> optional "duration" (Decode.map Just Decode.int) Nothing
        |> required "completed" completedDecoder
        |> optional "pace" (Decode.map Just Decode.int) Nothing
        |> optional "distance" (Decode.map Just Decode.float) Nothing
        |> optional "distanceUnits" (Decode.map Just distanceUnits.decoder) Nothing
        |> optional "race" (Decode.map Just raceDistance.decoder) Nothing
        |> optional "effort" (Decode.map Just effort.decoder) Nothing
        |> optional "emoji" (Decode.map Just Decode.string) Nothing


encoder : Activity -> Encode.Value
encoder activity =
    let
        maybeEncode fieldM encoder_ =
            case fieldM of
                Just field ->
                    encoder_ field

                Nothing ->
                    Encode.null

        encodeCompleted c =
            case c of
                Completed ->
                    Encode.bool True

                Planned ->
                    Encode.bool False

        activityDataFields data =
            [ ( "type", activityType.encode data.activityType )
            , ( "duration", maybeEncode data.duration Encode.int )
            , ( "completed", encodeCompleted data.completed )
            , ( "pace", maybeEncode data.pace Encode.int )
            , ( "distance", maybeEncode data.distance Encode.float )
            , ( "distanceUnits", maybeEncode data.distanceUnits distanceUnits.encode )
            , ( "race", maybeEncode data.race raceDistance.encode )
            , ( "effort", maybeEncode data.effort effort.encode )
            , ( "emoji", maybeEncode data.emoji Encode.string )
            ]

        dataEncoder laps planned =
            Encode.object <|
                [ ( "laps", Encode.list lapEncoder laps )
                , ( "planned", Encode.list lapEncoder planned )
                ]

        lapEncoder lapData =
            case lapData of
                Individual data ->
                    Encode.object
                        (activityDataFields data)

                Repeats count laps ->
                    Encode.object
                        [ ( "repeats", Encode.int count )
                        , ( "laps", Encode.list (\l -> Encode.object (activityDataFields l)) laps )
                        ]
    in
    Encode.object
        [ ( "id", Encode.string activity.id )
        , ( "date", Encode.string (Date.toIsoString activity.date) )
        , ( "description", Encode.string activity.description )
        , ( "data", dataEncoder activity.laps activity.planned )
        ]


dateDecoder : Decode.Decoder Date
dateDecoder =
    let
        isoStringDecoder str =
            case Date.fromIsoString str of
                Ok date ->
                    Decode.succeed date

                Err _ ->
                    Decode.fail "Invalid date string"
    in
    Decode.string
        |> Decode.andThen isoStringDecoder
