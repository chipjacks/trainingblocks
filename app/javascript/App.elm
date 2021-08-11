module App exposing (Env, Flags, document)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import UI.Toast
import UI.Util exposing (viewMaybe)


type alias Env =
    { title : String
    , rollbarAccessToken : String
    , environment : String
    , userId : Int
    , flash : Maybe String
    }


type alias Flags =
    { rollbar_access_token : String
    , environment : String
    , user_id : Int
    , flash : Maybe String
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
            , flash = flags.flash
            }

        viewFlash strM =
            viewMaybe strM
                (\str ->
                    UI.Toast.top
                        |> UI.Toast.withAttributes [ class "toast--fade" ]
                        |> UI.Toast.view (Html.text str)
                )
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
                { title = title ++ " | Rhino Log", body = [ viewFlash env.flash, view model ] }
        , subscriptions =
            \( env, model ) ->
                subscriptions model
        }
