module Api exposing (errorString, getActivities, getActivitiesResolver, getSettings, postActivities, postSettings)

import Activity
import Activity.Types exposing (Activity)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Pace.List exposing (PaceList)
import Task exposing (Task)
import Time exposing (Month(..))



-- ACTIVITIES


activitiesRoute =
    "/activities"


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
        , url = activitiesRoute
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| getActivitiesResolver
        , timeout = Nothing
        }


postActivities : String -> List ( String, Int ) -> List ( String, Activity ) -> Task Http.Error ( String, Bool )
postActivities revision orderUpdates activityUpdates =
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
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = activitiesRoute
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



-- SETTINGS


settingsRoute =
    "/settings"


getSettingsResolver =
    let
        paceDecoder =
            Decode.map2 Tuple.pair (Decode.field "name" Decode.string) (Decode.field "pace" Decode.int)
    in
    handleJsonResponse <|
        Decode.field "paces" (Decode.list paceDecoder)


getSettings : Task Http.Error (PaceList String)
getSettings =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = settingsRoute ++ ".json"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| getSettingsResolver
        , timeout = Nothing
        }


postSettings : PaceList String -> Task Http.Error Bool
postSettings paces =
    let
        pacesEncoder ( name, pace ) =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "pace", Encode.int pace )
                ]
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = settingsRoute
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "paces", Encode.list pacesEncoder paces )
                    ]
                )
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.field "ok" Decode.bool
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
