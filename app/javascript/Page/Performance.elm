module Page.Performance exposing (main)

import Activity
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, RaceDistance)
import Api
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import MonoIcons
import Task
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Performance | Rhino Log", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Task.attempt GotActivities Api.getActivities
    )


type alias Model =
    { races : List ActivityData
    }


type Msg
    = GotActivities (Result Http.Error ( String, List Activity ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActivities result ->
            case result of
                Ok ( _, activities ) ->
                    let
                        races =
                            List.concatMap Activity.Laps.listData activities
                                |> List.filter (\l -> l.race /= Nothing)
                    in
                    ( { model | races = races }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )


view model =
    let
        backButton =
            Html.a [ class "button row", style "align-items" "bottom", Html.Attributes.href "/calendar" ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , Html.text "Back"
                ]

        navHeader =
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Performance" ]
    in
    Skeleton.default
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withBackButton backButton
                |> Navbar.withItems [ navHeader ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody model =
    Html.text "loaded"
