module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI.Layout exposing (column, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Settings | Rhino Log", body = [ view model ] }
        , update = \model msg -> update model msg
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model placeholderPaces
    , Cmd.none
    )


placeholderPaces =
    [ ( "Very Easy", "8:00" )
    , ( "Easy", "7:00" )
    , ( "Moderate", "6:30" )
    , ( "Hard", "5:00" )
    ]


type alias Model =
    { trainingPaces : List ( String, String ) }


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
    Skeleton.default
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withBackButton backButton
                |> Navbar.withItems [ navHeader ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html msg
viewBody { trainingPaces } =
    column []
        [ Html.h3 [] [ Html.text "Training Paces" ]
        , viewTrainingPaces trainingPaces
        ]


viewTrainingPaces : List ( String, String ) -> Html msg
viewTrainingPaces paces =
    column [] (List.map viewPaceForm paces)


viewPaceForm : ( String, String ) -> Html msg
viewPaceForm ( name, pace ) =
    row []
        [ Html.input [ Html.Attributes.value name ] []
        , Html.input [ Html.Attributes.value pace ] []
        ]
