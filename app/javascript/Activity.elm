module Activity exposing (Activity, ActivityData, ActivityType(..), Effort(..), Id, RaceDistance(..), Seconds, activityType, decoder, effort, encoder, initActivityData, mprLevel, newId, raceDistance)

import Date exposing (Date)
import Enum exposing (Enum)
import Json.Decode as Decode
import Json.Encode as Encode
import MPRLevel
import Pace exposing (Pace)
import Random


type alias Activity =
    { id : Id
    , date : Date
    , description : String
    , data : ActivityData
    , laps : Maybe (List ActivityData)
    }


type alias ActivityData =
    { activityType : ActivityType
    , duration : Maybe Seconds
    , completed : Bool
    , pace : Maybe Pace
    , race : Maybe RaceDistance
    , effort : Maybe Effort
    , emoji : Maybe String
    }


initActivityData : ActivityData
initActivityData =
    ActivityData
        Run
        Nothing
        True
        Nothing
        Nothing
        Nothing
        Nothing


type ActivityType
    = Run
    | Other


activityType : Enum ActivityType
activityType =
    Enum.create
        [ ( "Run", Run )
        , ( "Other", Other )
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


mprLevel : Activity -> Maybe Int
mprLevel activity =
    case ( activity.data.race, activity.data.duration ) of
        ( Just distance_, Just duration ) ->
            MPRLevel.lookup MPRLevel.Neutral
                (raceDistance.toString distance_)
                duration
                |> Result.map (\( _, level ) -> level)
                |> Result.toMaybe

        _ ->
            Nothing


type alias Id =
    String


type alias Seconds =
    Int


type RaceDistance
    = FiveK
    | EightK
    | FiveMile
    | TenK
    | FifteenK
    | TenMile
    | TwentyK
    | HalfMarathon
    | TwentyFiveK
    | ThirtyK
    | Marathon


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
        ]


type Effort
    = Easy
    | Moderate
    | Hard


effort : Enum Effort
effort =
    Enum.create
        [ ( "Easy", Easy )
        , ( "Moderate", Moderate )
        , ( "Hard", Hard )
        ]



-- SERIALIZATION


decoder : Decode.Decoder Activity
decoder =
    Decode.map5 Activity
        (Decode.field "id" Decode.string)
        (Decode.field "date" dateDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "data" activityDataDecoder)
        (Decode.maybe (Decode.at [ "data", "laps" ] (Decode.list activityDataDecoder)))


activityDataDecoder : Decode.Decoder ActivityData
activityDataDecoder =
    Decode.map7 ActivityData
        (Decode.field "type" activityType.decoder)
        (Decode.maybe (Decode.field "duration" Decode.int))
        (Decode.field "completed" Decode.bool)
        (Decode.maybe (Decode.field "pace" Decode.int))
        (Decode.maybe (Decode.field "race" raceDistance.decoder))
        (Decode.maybe (Decode.field "effort" effort.decoder))
        (Decode.maybe (Decode.field "emoji" Decode.string))


encoder : Activity -> Encode.Value
encoder activity =
    let
        maybeEncode fieldM encoder_ =
            case fieldM of
                Just field ->
                    encoder_ field

                Nothing ->
                    Encode.null

        dataEncoder data laps =
            Encode.object
                [ ( "type", activityType.encode data.activityType )
                , ( "duration", maybeEncode data.duration Encode.int )
                , ( "completed", Encode.bool data.completed )
                , ( "pace", maybeEncode data.pace Encode.int )
                , ( "race", maybeEncode data.race raceDistance.encode )
                , ( "effort", maybeEncode data.effort effort.encode )
                , ( "emoji", maybeEncode data.emoji Encode.string )
                , ( "laps", maybeEncode laps (Encode.list lapEncoder) )
                ]

        lapEncoder data =
            Encode.object
                [ ( "type", activityType.encode data.activityType )
                , ( "duration", maybeEncode data.duration Encode.int )
                , ( "completed", Encode.bool data.completed )
                , ( "pace", maybeEncode data.pace Encode.int )
                , ( "race", maybeEncode data.race raceDistance.encode )
                , ( "effort", maybeEncode data.effort effort.encode )
                , ( "emoji", maybeEncode data.emoji Encode.string )
                ]
    in
    Encode.object
        [ ( "id", Encode.string activity.id )
        , ( "date", Encode.string (Date.toIsoString activity.date) )
        , ( "description", Encode.string activity.description )
        , ( "data", dataEncoder activity.data activity.laps )
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
