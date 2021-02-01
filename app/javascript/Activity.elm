module Activity exposing (Activity, ActivityData, Distance(..), Effort(..), Id, Seconds, decoder, distance, effort, encoder, mprLevel, newId)

import Date exposing (Date)
import Emoji
import Enum exposing (Enum)
import Json.Decode as Decode
import Json.Encode as Encode
import MPRLevel
import Pace exposing (Pace)
import Random
import Task exposing (Task)


type alias Activity =
    { id : Id
    , date : Date
    , description : String
    , data : ActivityData
    , sessionData : Maybe (List ActivityData)
    }


type alias ActivityData =
    { duration : Seconds
    , completed : Bool
    , pace : Maybe Pace
    , distance : Maybe Distance
    , effort : Maybe Effort
    , emoji : Maybe String
    }


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
    case activity.data.distance of
        Just distance_ ->
            MPRLevel.lookup MPRLevel.Neutral
                (distance.toString distance_)
                activity.data.duration
                |> Result.map (\( rt, level ) -> level)
                |> Result.toMaybe

        _ ->
            Nothing


type alias Id =
    String


type alias Seconds =
    Int


type Distance
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


distance : Enum Distance
distance =
    Enum.create
        [ FiveK
        , EightK
        , FiveMile
        , TenK
        , FifteenK
        , TenMile
        , TwentyK
        , HalfMarathon
        , TwentyFiveK
        , ThirtyK
        , Marathon
        ]
        (\a ->
            case a of
                FiveK ->
                    "5k"

                EightK ->
                    "8k"

                FiveMile ->
                    "5 mile"

                TenK ->
                    "10k"

                FifteenK ->
                    "15k"

                TenMile ->
                    "10 mile"

                TwentyK ->
                    "20k"

                HalfMarathon ->
                    "Half Marathon"

                TwentyFiveK ->
                    "25k"

                ThirtyK ->
                    "30k"

                Marathon ->
                    "Marathon"
        )


type Effort
    = Easy
    | Moderate
    | Hard


effort : Enum Effort
effort =
    Enum.create
        [ Easy
        , Moderate
        , Hard
        ]
        (\e ->
            case e of
                Easy ->
                    "Easy"

                Moderate ->
                    "Moderate"

                Hard ->
                    "Hard"
        )



-- SERIALIZATION


decoder : Decode.Decoder Activity
decoder =
    Decode.map5 Activity
        (Decode.field "id" Decode.string)
        (Decode.field "date" dateDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "data" activityDataDecoder)
        (Decode.maybe (Decode.field "sessionData" (Decode.list activityDataDecoder)))


activityDataDecoder : Decode.Decoder ActivityData
activityDataDecoder =
    Decode.map6 ActivityData
        (Decode.field "duration" Decode.int)
        (Decode.field "completed" Decode.bool)
        (Decode.maybe (Decode.field "pace" Decode.int))
        (Decode.maybe (Decode.field "distance" distance.decoder))
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

        dataEncoder data =
            Encode.object
                [ ( "duration", Encode.int data.duration )
                , ( "completed", Encode.bool data.completed )
                , ( "pace", maybeEncode data.pace Encode.int )
                , ( "distance", maybeEncode data.distance distance.encode )
                , ( "effort", maybeEncode data.effort effort.encode )
                , ( "emoji", maybeEncode data.emoji Encode.string )
                ]
    in
    Encode.object
        [ ( "id", Encode.string activity.id )
        , ( "date", Encode.string (Date.toIsoString activity.date) )
        , ( "description", Encode.string activity.description )
        , ( "data", dataEncoder activity.data )
        , ( "sessionData", maybeEncode activity.sessionData (Encode.list dataEncoder) )
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
