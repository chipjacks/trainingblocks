module Activity exposing (Activity, list, decoder, ActivityType(..), groupByType, pace, miles)

import Http exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, custom)
import Date exposing (Date)


type alias Activity =
    { id : Maybe String
    , name : String
    , distance : Float
    , duration : Int
    , type_ : ActivityType
    , startDate : Date
    , completed : Bool
    , externalId : Maybe String
    }


type ActivityType
    = Run
    | Ride
    | Weights
    | Swim
    | Other


list : Date -> Date -> Request (List Activity)
list startDate endDate =
    let
        request_url =
            url "/activities"
                [ ( "before", ((Date.toTime endDate / 1000) |> toString) ) ]
    in
        Http.get request_url (JD.list decoder)


decoder : Decoder Activity
decoder =
    decode Activity
        |> optional "id" (JD.nullable JD.string) Nothing
        |> required "name" JD.string
        |> required "distance" JD.float
        |> required "duration" JD.int
        |> custom (JD.field "type_" JD.string |> JD.andThen (JD.succeed << typeFromString))
        |> custom (JD.field "start_date" JD.string |> JD.andThen (fromResult << Date.fromString))
        |> required "completed" JD.bool
        |> optional "external_id" (JD.nullable JD.string) Nothing


groupByType : List Activity -> List (List Activity)
groupByType activities =
    activityTypes
        |> List.map (\t -> List.filter (\a -> a.type_ == t) activities)
        |> List.filter (\l -> List.length l > 0)


activityTypes : List ActivityType
activityTypes =
    [ Run, Ride, Weights, Swim, Other ]


pace : Activity -> String
pace activity =
    let
        minPerMile =
            (((toFloat activity.duration) / 60) / (activity.distance / 1609.34))

        mins =
            floor minPerMile

        secs =
            round ((minPerMile - (toFloat mins)) * 60)

        strSecs =
            if secs < 10 then
                "0" ++ (toString secs)
            else if secs == 60 then
                "00"
            else
                toString secs

        strMins =
            if secs == 60 then
                toString (mins + 1)
            else
                toString mins
    in
        if activity.distance == 0 then
            "unknown pace"
        else
            strMins ++ ":" ++ strSecs ++ " pace"


miles : Activity -> String
miles activity =
    (activity.distance / 1609 * 10)
        |> round
        |> toFloat
        |> (\n -> n / 10)
        |> (\n -> toString n ++ " miles")



-- INTERNAL


url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.encodeUri key ++ "=" ++ Http.encodeUri value


typeFromString : String -> ActivityType
typeFromString str =
    case str of
        "Run" ->
            Run

        "Ride" ->
            Ride

        "WeightTraining" ->
            Weights

        "Swim" ->
            Swim

        _ ->
            Other


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            JD.succeed a

        Err errorMessage ->
            JD.fail errorMessage
