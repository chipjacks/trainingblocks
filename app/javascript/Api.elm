module Api exposing (errorString, getActivities, getActivitiesResolver, postActivities)

import Activity
import Activity.Types exposing (Activity)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time exposing (Month(..))



-- CONFIG


storeUrl =
    "/activities"



-- ROUTES


getActivitiesResolver =
    handleJsonResponse <|
        Decode.map2 Tuple.pair
            (Decode.field "rev" Decode.string)
            (Decode.field "activities" (Decode.list Activity.decoder))


getActivities : Task Http.Error ( String, List Activity )
getActivities =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = storeUrl
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| getActivitiesResolver
        , timeout = Nothing
        }


postActivities : String -> String -> List ( String, Int ) -> List ( String, Activity ) -> Task Http.Error ( String, Bool )
postActivities csrfToken revision orderUpdates activityUpdates =
    let
        activityUpdateEncoder ( msg, activity ) =
            Encode.object
                [ ( "msg", Encode.string msg )
                , ( "activity", Activity.encoder activity )
                ]

        orderUpdateEncoder ( id, order ) =
            Encode.object
                [ ( "id", Encode.string id )
                , ( "order", Encode.int order )
                ]
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "X-CSRF-Token" csrfToken ]
        , url = storeUrl
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "orderUpdates", Encode.list orderUpdateEncoder orderUpdates )
                    , ( "activityUpdates", Encode.list activityUpdateEncoder activityUpdates )
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
