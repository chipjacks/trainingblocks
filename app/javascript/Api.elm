module Api exposing (errorString, getActivities, postActivities)

import Activity exposing (Activity)
import Date
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task exposing (Task)
import Time exposing (Month(..), utc)



-- CONFIG


storeUrl =
    "/activities"



-- ROUTES


getActivities : Task Http.Error ( String, List Activity )
getActivities =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = storeUrl
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.map2 Tuple.pair
                        (Decode.field "rev" Decode.string)
                        (Decode.field "activities" (Decode.list Activity.decoder))
        , timeout = Nothing
        }


postActivities : String -> String -> List Activity -> List ( String, Activity ) -> Task Http.Error ( String, Bool )
postActivities csrfToken revision activities changes =
    let
        changeEncoder ( msg, activity ) =
            Encode.object
                [ ( "msg", Encode.string msg )
                , ( "activity", Activity.encoder activity )
                ]

        entryEncoder activity =
            Encode.object
                [ ( "date", Encode.string (Date.toIsoString activity.date) )
                , ( "id", Encode.string activity.id )
                ]
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "X-CSRF-Token" csrfToken ]
        , url = storeUrl
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "entries", Encode.list entryEncoder activities )
                    , ( "changes", Encode.list changeEncoder changes )
                    , ( "rev", Encode.string revision )
                    ]
                )
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.map2 Tuple.pair
                        (Decode.field "rev" Decode.string)
                        (Decode.field "ok" Decode.bool)
        , timeout = Nothing
        }


errorString : Http.Error -> String
errorString result =
    case result of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.BadStatus statusCode ->
            "Bad status code: " ++ String.fromInt statusCode

        Http.NetworkError ->
            "Network error"

        Http.BadBody decoderError ->
            decoderError



-- INTERNAL


handleJsonResponse : Decode.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err decoderErr ->
                    Err (Http.BadBody (Decode.errorToString decoderErr))

                Ok result ->
                    Ok result
