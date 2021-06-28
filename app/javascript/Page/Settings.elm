module Page.Settings exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import MonoIcons
import Pace
import Ports
import Selection exposing (Selection)
import UI.Button as Button
import UI.Input
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import UI.Util exposing (attributeIf, attributeMaybe, onPointerMove, styleIf, viewMaybe)
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
    ( Model (Selection.init placeholderPaces) Nothing
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
                { name = Validate.init Ok name
                , pace = Validate.init Validate.parsePace pace
                , dragging = False
                }
            )


type alias Model =
    { trainingPaces : Selection PaceForm
    , dragging : Maybe ( Float, Float )
    }


type alias PaceForm =
    { name : Validate.Field String String
    , pace : Validate.Field String Int
    , dragging : Bool
    }


type Msg
    = EditedPace Int String
    | EditedName Int String
    | ClickedAddPace
    | ClickedRemovePace Int
    | ClickedDragPace Int Int
    | BlurredPace
    | PointerMoved Float Float
    | PointerUp
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        EditedPace index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (\form -> { form | pace = Validate.update str form.pace })
            in
            ( { model | trainingPaces = newTrainingPaces }
            , Cmd.none
            )

        EditedName index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (\form -> { form | name = Validate.update str form.name })
            in
            ( { model | trainingPaces = newTrainingPaces }, Cmd.none )

        ClickedAddPace ->
            ( { model | trainingPaces = Selection.add newTrainingPace model.trainingPaces }
            , Cmd.none
            )

        ClickedRemovePace index ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.delete
            in
            ( { model | trainingPaces = newTrainingPaces }
            , Cmd.none
            )

        ClickedDragPace index pointerId ->
            ( { model
                | trainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (\form -> { form | dragging = True })
                , dragging = Just ( -100, -100 )
              }
            , Ports.setPointerCapture { targetId = config.trainingPaceListId, pointerId = pointerId }
            )

        PointerMoved x y ->
            ( { model | dragging = Just ( x, y ) }
            , Cmd.none
            )

        PointerUp ->
            ( { model
                | dragging = Nothing
                , trainingPaces = Selection.update (\form -> { form | dragging = False }) model.trainingPaces
              }
            , Cmd.none
            )

        BlurredPace ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


newTrainingPace : PaceForm
newTrainingPace =
    { name = Validate.init Ok ""
    , pace = Validate.init Validate.parsePace ""
    , dragging = False
    }


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
viewBody { trainingPaces, dragging } =
    let
        ( draggedPaces, paces ) =
            Selection.toList trainingPaces
                |> List.partition .dragging
    in
    column
        [ attributeMaybe dragging (\_ -> style "touch-action" "none")
        ]
        [ Html.h3 [] [ Html.text "Training Paces" ]
        , row []
            [ viewTrainingPaces dragging (Selection.selectedIndex trainingPaces) paces
            , column [] []
            ]
        , Maybe.map2
            (\position draggedPace ->
                viewDraggedPace position draggedPace
            )
            dragging
            (List.head draggedPaces)
            |> Maybe.withDefault (row [] [])
        ]


config =
    { sliderHeight = 500
    , maxPace = 9 * 60
    , minPace = 3 * 60
    , trainingPaceListId = "training-pace-list"
    }


viewTrainingPaces : Maybe ( Float, Float ) -> Int -> List PaceForm -> Html Msg
viewTrainingPaces dragging selectedIndex paces =
    column
        [ Html.Attributes.id config.trainingPaceListId
        , attributeMaybe dragging (\_ -> onPointerMove PointerMoved)
        , attributeMaybe dragging (\_ -> Html.Events.on "pointerup" (Decode.succeed PointerUp))
        , style "height" (String.fromInt config.sliderHeight ++ "px")
        , style "position" "relative"
        ]
        (viewAddButton :: List.indexedMap viewPaceForm paces)


viewPaceForm : Int -> PaceForm -> Html Msg
viewPaceForm index { name, pace, dragging } =
    let
        { minPace, maxPace, sliderHeight } =
            config

        yOffset =
            Result.map (\seconds -> (1 - (toFloat seconds - minPace) / (maxPace - minPace)) * sliderHeight) pace.result
                |> Result.withDefault (sliderHeight / 2)
    in
    row
        [ style "margin-top" "5px"
        , style "margin-bottom" "5px"
        , styleIf (not dragging) "position" "absolute"
        , styleIf (not dragging) "top" (String.fromFloat yOffset ++ "px")
        ]
        [ Button.action "Drag" MonoIcons.drag NoOp
            |> Button.withAttributes
                [ class "row__button--drag"
                , Html.Events.on "pointerdown" (Decode.map (ClickedDragPace index) (Decode.field "pointerId" Decode.int))
                ]
            |> Button.withAppearance Button.Small Button.Subtle Button.None
            |> Button.view
        , UI.Input.pace (EditedPace index)
            |> (\inputConfig ->
                    case pace.result of
                        Err Validate.MissingError ->
                            inputConfig

                        Err err ->
                            UI.Input.withError err inputConfig

                        _ ->
                            inputConfig
               )
            |> UI.Input.withPlaceholder (Result.map Pace.paceToString pace.result |> Result.withDefault "mm:ss")
            |> UI.Input.withAttributes [ Html.Events.onBlur BlurredPace ]
            |> UI.Input.view pace.value
        , compactColumn [ style "width" "10px" ] []
        , UI.Input.text (EditedName index)
            |> UI.Input.withResultError name.result
            |> UI.Input.view name.value
        , Button.action "Remove Pace" MonoIcons.remove (ClickedRemovePace index)
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]


viewDraggedPace : ( Float, Float ) -> PaceForm -> Html Msg
viewDraggedPace ( x, y ) paceForm =
    Html.div
        [ style "position" "absolute"
        , style "top" (String.fromFloat (y - 20) ++ "px")
        , style "touch-action" "none"
        , Html.Attributes.id "dragged-element"
        ]
        [ viewPaceForm 0 paceForm
        ]


viewAddButton : Html Msg
viewAddButton =
    row [ style "justify-content" "flex-end" ]
        [ Button.action "Add Pace" MonoIcons.add ClickedAddPace
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]
