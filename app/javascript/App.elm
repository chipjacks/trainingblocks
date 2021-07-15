module App exposing (Env, document)

import Browser
import Html exposing (Html)
import Report


type alias Env =
    { report : Report.Reporter
    }


type alias Document model msg effect =
    { title : String
    , init : ( model, effect )
    , update : Env -> msg -> model -> ( model, effect )
    , perform : effect -> Cmd msg
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    }


document : Document model msg effect -> Program String ( Env, model ) msg
document { title, init, update, perform, view, subscriptions } =
    let
        initEnv flags =
            { report = Report.init title flags }
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
