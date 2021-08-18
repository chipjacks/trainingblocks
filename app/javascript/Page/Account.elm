module Page.Account exposing (main)

import Api
import App
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Task
import UI.Layout exposing (column, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import User exposing (User)


main : Program App.Flags ( App.Env, Model ) Msg
main =
    App.document
        { title = "Account"
        , init = init
        , update = update
        , perform = identity
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model "" False
    , Task.attempt GotUser Api.getUser
    )


type alias Model =
    { email : String
    , stravaImport : Bool
    }


type Msg
    = GotUser (Result Http.Error User)


update : App.Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case msg of
        GotUser result ->
            case result of
                Ok user ->
                    ( { model | email = user.email, stravaImport = user.provider == Just "strava" }
                    , Cmd.none
                    )

                Err err ->
                    Debug.todo "error"


view : Model -> Html Msg
view model =
    let
        navHeader =
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Account" ]
    in
    Skeleton.default
        |> Skeleton.withTitle "Account"
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withItems [ navHeader ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html Msg
viewBody { email, stravaImport } =
    column []
        [ Html.text email
        , row []
            [ Html.text "Strava Import"
            , if stravaImport then
                Html.text "Connected"

              else
                Html.text "Disconnected"
            ]
        ]
