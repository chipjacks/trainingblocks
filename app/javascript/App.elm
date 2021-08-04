module App exposing (Env, Flags, document)

import Browser
import Html exposing (Html)


type alias Env =
    { title : String
    , rollbarAccessToken : String
    , environment : String
    , userId : Int
    }


type alias Flags =
    { rollbar_access_token : String
    , environment : String
    , user_id : Int
    }


type alias Document model msg effect =
    { title : String
    , init : ( model, effect )
    , update : Env -> msg -> model -> ( model, effect )
    , perform : effect -> Cmd msg
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


document : Document model msg effect -> Program Flags ( Env, model ) msg
document { title, init, update, perform, view, subscriptions } =
    let
        initEnv flags =
            { title = title
            , rollbarAccessToken = flags.rollbar_access_token
            , environment = flags.environment
            , userId = flags.user_id
            }
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