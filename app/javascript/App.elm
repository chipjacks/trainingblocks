module App exposing (Env, document)

import Browser
import Html exposing (Html)
import Json.Decode as Decode


type alias Env =
    { title : String
    , rollbarAccessToken : String
    , environment : String
    , userId : Int
    }


type alias Document model msg effect =
    { title : String
    , init : ( model, effect )
    , update : Env -> msg -> model -> ( model, effect )
    , perform : effect -> Cmd msg
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


decoder : String -> Decode.Decoder Env
decoder title =
    Decode.map4 Env
        (Decode.succeed title)
        (Decode.field "rollbar_access_token" Decode.string)
        (Decode.field "environment" Decode.string)
        (Decode.field "user_id" Decode.int)


document : Document model msg effect -> Program String ( Env, model ) msg
document { title, init, update, perform, view, subscriptions } =
    let
        initEnv flags =
            Decode.decodeString (decoder title) flags
                |> Result.withDefault (Env title "" "" 0)
    in
    Browser.document
        { init =
            \flags ->
                init
                    |> Tuple.mapFirst (\model -> ( initEnv flags, model ))
                    |> Tuple.mapSecond perform
        , update =
            \msg ( env, model ) ->
                update env msg model
                    |> Tuple.mapFirst (\m -> ( env, m ))
                    |> Tuple.mapSecond perform
        , view =
            \( env, model ) ->
                { title = title ++ " | Rhino Log", body = [ view model ] }
        , subscriptions =
            \( env, model ) ->
                subscriptions model
        }
