module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI.Layout exposing (column, expandingRow)
import UI.Navbar as Navbar


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Settings | Rhino Log", body = [ view model ] }
        , update = \model msg -> update model msg
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( None
    , Cmd.none
    )


type Model
    = None


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    let
        backButton =
            Html.a [ class "button row", style "align-items" "bottom", Html.Attributes.href "/calendar" ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , Html.text "Back"
                ]

        navHeader =
            Html.div [ style "font-size" "1.3rem" ] [ Html.text "Settings" ]
    in
    column
        [ style "height" "100vh", style "width" "100vw" ]
        [ Navbar.default
            |> Navbar.withBackButton backButton
            |> Navbar.withItems [ navHeader ]
            |> Navbar.view
        , expandingRow []
            [ column [ class "container" ]
                [ Html.text "Settings" ]
            ]
        ]
