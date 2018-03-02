module HumanAPI exposing (..)

import Http exposing (..)
import Json.Decode exposing (Decoder, decodeString, int, float, list, field, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, resolve)
import Msgs exposing (Msg)
import Models exposing (HumanAPIActivity, Activity)
import Date exposing (Date, Month(..))
import RemoteData exposing (RemoteData)
import Date.Extra as Date


accessToken : String
accessToken =
    "556179d8857cb13944c30038aba96a3565bafa22"


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

        request_url =
            url "https://api.humanapi.co/v1/human/activities"
                [ ( "access_token", accessToken )
                , ( "start_date", (Date.toFormattedString "y-MM-dd" startDate) )
                , ( "end_date", (Date.toFormattedString "y-MM-dd" endDate) )
                ]
    in
        Http.get request_url (list activityDecoder) |> RemoteData.sendRequest |> Cmd.map (Msgs.AActivitiesMsg << (Msgs.GotActivities (Date.toRataDie startDate)))


activityDecoder : Decoder HumanAPIActivity
activityDecoder =
    let
        toDecoder : String -> Int -> Float -> Decoder HumanAPIActivity
        toDecoder startDate duration distance =
            let
                date =
                    Date.fromIsoString startDate
            in
                case date of
                    Just date ->
                        succeed (HumanAPIActivity date duration distance)

                    Nothing ->
                        fail "This JSON is invalid"
    in
        decode toDecoder
            |> required "startTime" string
            |> required "duration" int
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


toActivity : HumanAPIActivity -> Activity
toActivity activity =
    Activity (toIntensity activity) (activity.duration // 60)


toMiles : Float -> Float
toMiles meters =
    meters / 1609.34


toIntensity : HumanAPIActivity -> Int
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