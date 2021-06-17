module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import MonoIcons
import Pace
import Selection exposing (Selection)
import UI.Input
import UI.Layout exposing (column, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import Validate exposing (Field)


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Settings | Rhino Log", body = [ view model ] }
        , update = \model msg -> update model msg
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model (Selection.init placeholderPaces)
    , Cmd.none
    )


placeholderPaces =
    [ ( "Very Easy", "8:00" )
    , ( "Easy", "7:00" )
    , ( "Moderate", "6:30" )
    , ( "Hard", "5:00" )
    ]
        |> List.map
            (\( name, pace ) ->
                ( Validate.init Ok name
                , Validate.init Validate.parsePace pace
                )
            )


type alias Model =
    { trainingPaces : Selection ( Validate.Field String String, Validate.Field String Int ) }


type Msg
    = EditedPace Int String
    | EditedName Int String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        EditedPace index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (Tuple.mapSecond (Validate.update str))
            in
            ( { model | trainingPaces = newTrainingPaces }, Cmd.none )

        EditedName index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (Tuple.mapFirst (Validate.update str))
            in
            ( { model | trainingPaces = newTrainingPaces }, Cmd.none )


view : Model -> Html Msg
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


viewBody : Model -> Html Msg
viewBody { trainingPaces } =
    column []
        [ Html.h3 [] [ Html.text "Training Paces" ]
        , viewTrainingPaces (Selection.toList trainingPaces)
        ]


viewTrainingPaces : List ( Field String String, Field String Int ) -> Html Msg
viewTrainingPaces paces =
    column [] (List.indexedMap viewPaceForm paces)


viewPaceForm : Int -> ( Field String String, Field String Int ) -> Html Msg
viewPaceForm index ( name, pace ) =
    row []
        [ Html.input [ Html.Attributes.value name.value, Html.Events.onInput (EditedName index) ] []
        , UI.Input.pace pace.value (EditedPace index) (pace.result |> Result.map Pace.paceToString)
        ]
