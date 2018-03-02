module StravaAPI exposing (..)

import Http exposing (..)
import Json.Decode exposing (Decoder, decodeString, int, float, list, field, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, resolve)
import Msgs exposing (Msg)
import Models exposing (Activity)
import Date exposing (Date, Month(..))
import RemoteData exposing (RemoteData)
import Date.Extra as Date

type alias StravaAPIActivity =
    { -- id : String,
      date : Date
    , duration : Int
    , distance : Float
    }


loadMonthsActivities : Date -> Cmd Msg
loadMonthsActivities date =
    Date.range Date.Week 1 (Date.floor Date.Month date) (Date.add Date.Month 1 date) |> List.map loadWeeksActivities |> Cmd.batch


loadWeeksActivities : Date -> Cmd Msg
loadWeeksActivities date =
    let
        startDate =
            date

        endDate =
            Date.add Date.Week 1 date

        request_url = url "/activities"
                [ ( "before", ((Date.toTime endDate / 1000) |> toString) ) ]
    in
        Http.get request_url (list activityDecoder) |> RemoteData.sendRequest |> Cmd.map (Msgs.AActivitiesMsg << (Msgs.GotActivities (Date.toRataDie startDate)))


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


toActivity : StravaAPIActivity -> Activity
toActivity activity =
    Activity (toIntensity activity) (activity.duration // 60)


toMiles : Float -> Float
toMiles meters =
    meters / 1609.34


toIntensity : StravaAPIActivity -> Int
toIntensity activity =
    let
        -- TODO: what about activites that aren't runs?
        minPerMile =
            (((toFloat activity.duration) / 60) / (toMiles activity.distance))

        estimatedIntensity =
            (9 - Basics.floor minPerMile)
    in
        if estimatedIntensity < 1 then
            1
        else if estimatedIntensity > 5 then
            5
        else
            estimatedIntensity