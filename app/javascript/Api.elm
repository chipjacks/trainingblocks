module Api exposing (getActivities, postActivities)

import Activity exposing (Activity)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task exposing (Task)
import Time exposing (Month(..), utc)



-- CONFIG


config =
    { docId = "428e9b77627a652f297c35eedca65c95"
    , url = "https://6483b615-f5bc-4f3d-8b78-188c8df679dc-bluemix.cloudantnosqldb.appdomain.cloud"
    , database =
        { dev = "runapp2_dev"
        , prod = "runapp2"
        }
    , authHeader =
        { dev = "YXBpa2V5LTU2NGQ2NWMxNWNkNTRhYjRiOGVhOTUxMmU2YmU4MmE0OjI4YjgxMWZhMDExZGZiYjVlOThlNzdmNmYzMTdmMDZkM2Y1YTJmNGE="
        , prod = "YXBpa2V5LWZiZTQyOTUwZWE2YzQxNGVhZjRiNGM4MmZkOGQ3MDgyOjNiNmRiMDMzZTM5NzlkZWQzMDhmNzM5YTgyNzdhZjNhZjM0YjdlZDU="
        }
    }


storeUrl =
    String.join "/" [ config.url, config.database.dev, config.docId ]


authHeader =
    Http.header "Authorization" ("Basic " ++ config.authHeader.dev)



-- ROUTES


getActivities : Task String ( String, List Activity )
getActivities =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Content-Type" "application/json", authHeader ]
        , url = storeUrl
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    Decode.map2 Tuple.pair
                        (Decode.field "_rev" Decode.string)
                        (Decode.field "activities" (Decode.list Activity.decoder))
        , timeout = Nothing
        }


postActivities : String -> List Activity -> Task String Bool
postActivities revision activities =
    Http.task
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json", authHeader ]
        , url = storeUrl
        , body = Http.jsonBody (Encode.object [ ( "_rev", Encode.string revision ), ( "activities", Encode.list Activity.encoder activities ) ])
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
