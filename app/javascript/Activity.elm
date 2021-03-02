module Activity exposing (Activity, ActivityData, ActivityType(..), Completion(..), Effort(..), Id, LapData(..), RaceDistance(..), Seconds, activityType, decoder, effort, encoder, initActivityData, listLapData, mprLevel, newId, raceDistance)

import Date exposing (Date)
import Enum exposing (Enum)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import MPRLevel
import Pace exposing (Pace)
import Random


type alias Activity =
    { id : Id
    , date : Date
    , description : String
    , data : ActivityData
    , laps : Maybe (List LapData)
    }


type alias ActivityData =
    { activityType : ActivityType
    , duration : Maybe Seconds
    , completed : Completion
    , pace : Maybe Pace
    , race : Maybe RaceDistance
    , effort : Maybe Effort
    , emoji : Maybe String
    }


type LapData
    = Individual ActivityData
    | Repeats Int (List ActivityData)


initActivityData : ActivityData
initActivityData =
    ActivityData
        Run
        Nothing
        Completed
        Nothing
        Nothing
        Nothing
        Nothing


type Completion
    = Completed
    | Planned


type ActivityType
    = Run
    | Other


activityType : Enum ActivityType
activityType =
    Enum.create
        [ ( "Run", Run )
        , ( "Other", Other )
        ]


listLapData : Activity -> List ActivityData
listLapData activity =
    activity.laps
        |> Maybe.withDefault [ Individual activity.data ]
        |> List.concatMap
            (\lap ->
                case lap of
                    Individual data ->
                        List.singleton data

                    Repeats _ list ->
                        []
            )


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
    Decode.succeed Activity
        |> required "id" Decode.string
        |> required "date" dateDecoder
        |> required "description" Decode.string
        |> required "data" activityDataDecoder
        |> custom (Decode.maybe (Decode.at [ "data", "laps" ] (Decode.list lapDataDecoder)))


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
                        case c of
                            True ->
                                Decode.succeed Completed

                            False ->
                                Decode.succeed Planned
                    )
    in
    Decode.succeed ActivityData
        |> required "type" activityType.decoder
        |> optional "duration" (Decode.map Just Decode.int) Nothing
        |> required "completed" completedDecoder
        |> optional "pace" (Decode.map Just Decode.int) Nothing
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
            , ( "race", maybeEncode data.race raceDistance.encode )
            , ( "effort", maybeEncode data.effort effort.encode )
            , ( "emoji", maybeEncode data.emoji Encode.string )
            ]

        dataEncoder data laps =
            Encode.object <|
                activityDataFields data
                    ++ [ ( "laps", maybeEncode laps (Encode.list lapEncoder) ) ]

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
