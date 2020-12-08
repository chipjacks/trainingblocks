module Api exposing (getActivities, postActivities)

import Activity exposing (Activity)
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


getActivities : Task String (List Activity)
getActivities =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = storeUrl
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.list Activity.decoder
        , timeout = Nothing
        }


postActivities : String -> List Activity -> Task String Bool
postActivities csrfToken activities =
    Http.task
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "X-CSRF-Token" csrfToken ]
        , url = storeUrl
        , body = Http.jsonBody (Encode.object [ ( "activities", Encode.list Activity.encoder activities ) ])
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.field "ok" Decode.bool
        , timeout = Nothing
        }



-- INTERNAL


handleJsonResponse : Decode.Decoder a -> Http.Response String -> Result String a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err ("Bad URL: " ++ url)

        Http.Timeout_ ->
            Err "Timeout"

        Http.BadStatus_ { statusCode } _ ->
            Err ("Bad status code: " ++ String.fromInt statusCode)

        Http.NetworkError_ ->
            Err "Network error"

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err decoderErr ->
                    Err (Decode.errorToString decoderErr)

                Ok result ->
                    Ok result
