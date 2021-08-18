module Api exposing (developerError, getActivities, getSettings, postActivities, putSettings, userError, getUser)

import Activity
import Activity.Types exposing (Activity)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Settings exposing (Settings)
import Task exposing (Task)
import Time exposing (Month(..))
import User exposing (User)



-- ACTIVITIES


activitiesRoute : String
activitiesRoute =
    "/activities"


getActivitiesResolver : Http.Response String -> Result Http.Error ( String, List Activity )
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


settingsRoute : String
settingsRoute =
    "/settings"


getSettingsResolver : Http.Response String -> Result Http.Error (Maybe Settings)
getSettingsResolver =
    handleJsonResponse
        (Decode.oneOf
            [ Settings.decoder |> Decode.map Just
            , Decode.succeed Nothing
            ]
        )


getSettings : Task Http.Error (Maybe Settings)
getSettings =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = settingsRoute ++ ".json"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| getSettingsResolver
        , timeout = Nothing
        }


putSettings : Settings -> Task Http.Error Bool
putSettings settings =
    Http.task
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = settingsRoute
        , body =
            Http.jsonBody
                (Settings.encoder settings)
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.field "ok" Decode.bool
        , timeout = Nothing
        }



-- USERS


usersRoute : String
usersRoute =
    "/users/edit"


getUser : Task Http.Error User
getUser =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = usersRoute ++ ".json"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| getUserResolver
        , timeout = Nothing
        }


getUserResolver : Http.Response String -> Result Http.Error User
getUserResolver =
    handleJsonResponse User.decoder



-- ERRORS


userError : Http.Error -> String
userError error =
    case error of
        Http.BadBody decoderError ->
            "Internal error"

        Http.BadUrl url ->
            "Internal error"

        _ ->
            developerError error


developerError : Http.Error -> String
developerError error =
    case error of
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
