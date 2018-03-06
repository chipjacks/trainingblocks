module StravaAPI exposing (..)

import Http exposing (..)
import Json.Decode exposing (Decoder, decodeString, int, float, list, field, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, resolve)
import Date exposing (Date, Month(..))
import Date.Extra as Date


type alias StravaAPIActivity =
    { -- id : String,
      date : Date
    , duration : Int
    , distance : Float
    }


listActivities : Date -> Date -> Request (List StravaAPIActivity)
listActivities startDate endDate =
    let
        request_url =
            url "/activities"
                [ ( "before", ((Date.toTime endDate / 1000) |> toString) ) ]
    in
        Http.get request_url (list activityDecoder)


activityDecoder : Decoder StravaAPIActivity
activityDecoder =
    let
        toDecoder : String -> Int -> Float -> Decoder StravaAPIActivity
        toDecoder startDate duration distance =
            let
                date =
                    Date.fromIsoString startDate
            in
                case date of
                    Just date ->
                        succeed (StravaAPIActivity date duration distance)

                    Nothing ->
                        fail "This JSON is invalid"
    in
        decode toDecoder
            |> required "start_date" string
            |> required "elapsed_time" int
            |> required "distance" float
            |> resolve


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
