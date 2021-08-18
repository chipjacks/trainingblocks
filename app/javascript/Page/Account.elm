module Page.Account exposing (main)

import Api
import App
import Html exposing (Html, a)
import Html.Attributes exposing (class, style)
import Http
import Task
import UI.Layout exposing (column, compactColumn, row)
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
    column [ style "margin" "15px", style "margin-top" "40px", style "margin-bottom" "40px" ]
        [ row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ Html.h3 [] [ Html.text "Email" ]
                , Html.text "Used for important communications about your account."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ Html.text "chipjacks@gmail.com"
                , a [] [ Html.text "Log out" ]
                , a [] [ Html.text "Update email or password" ]
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [] [ Html.text "Strava Import" ]
                    ]
                , Html.text "Link your Strava account to automatically import activities."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ Html.text "Connected"
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [] [ Html.text "Cancel my account" ]
                    ]
                , Html.text "This will delete your data and cannot be undone."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ a [] [ Html.text "Cancel my account" ]
                ]
            ]
        ]
