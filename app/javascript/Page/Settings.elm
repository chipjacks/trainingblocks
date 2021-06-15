module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import UI.Layout exposing (column)
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
    column
        [ style "height" "100vh", style "width" "100vw", style "position" "absolute" ]
        [ Navbar.view Navbar.default
        , Html.text "Settings"
        ]
