module Report exposing (init, data, withField, Reporter)

import Rollbar exposing (Rollbar)
import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

type alias Configs =
    { token : String
    , environment : String
    }


type alias Reporter
    = Rollbar


decoder : Decoder Configs
decoder =
    Decode.map2 Configs
        (Decode.field "rollbar_access_token" Decode.string)
        (Decode.field "environment" Decode.string)


init : String -> String -> Reporter
init scope json =
    let
        configs = 
            case Decode.decodeString decoder json of
                Err err ->
                    let
                        debug = Debug.log "error" (Decode.errorToString err)
                    in
                    Err err
                        |> Result.withDefault (Configs "" "")

                Ok res ->
                    res
    in
    Rollbar.scoped
        (Rollbar.token configs.token)
        (Rollbar.codeVersion "")
        (Rollbar.environment configs.environment)
        scope


data : Dict String Encode.Value
data =
    Dict.empty


withField : String -> Encode.Value -> Dict String Encode.Value -> Dict String Encode.Value
withField str val dict = 
    Dict.insert str val dict
