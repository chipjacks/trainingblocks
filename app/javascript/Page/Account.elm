module Page.Account exposing (main)

import Api
import App
import Html exposing (Html, a)
import Html.Attributes exposing (class, href, style)
import Http
import MonoIcons
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
                    Debug.todo "error" err


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
                [ Html.h3 [] [ Html.text email ]
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile", style "margin-top" "15px" ]
                [ a
                    [ class "button button--primary row"
                    , href "/users/sign_out"
                    , Html.Attributes.attribute "data-method" "delete"
                    , style "align-items" "center"
                    , style "width" "fit-content"
                    , style "margin-bottom" "10px"
                    ]
                    [ MonoIcons.icon (MonoIcons.logOut "white")
                    , compactColumn [ style "width" "10px" ] []
                    , Html.text "Log out"
                    ]
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ Html.h3 [] [ Html.text "Account Details" ]
                , Html.text "Update email or password, or cancel your account."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile", style "margin-top" "15px" ]
                [ a [ href "/users/edit" ] [ Html.text "Edit account" ]
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ Html.h3 [] [ Html.text "Strava Import" ]
                , Html.text "Link your Strava account to automatically import activities."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile", style "margin-top" "15px" ]
                (if stravaImport then
                    [ Html.text "Connected"
                    , a [] [ Html.text "Disconnect" ]
                    ]

                 else
                    [ Html.text "Not connected"
                    , a [] [ Html.text "Connect" ]
                    ]
                )
            ]
        ]
