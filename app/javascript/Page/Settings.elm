module Page.Settings exposing (main)

import Api
import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Http
import Json.Decode as Decode
import MonoIcons
import Pace
import Ports
import Selection exposing (Selection)
import Task
import UI.Button as Button
import UI.Input
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import UI.Util exposing (attributeIf, attributeMaybe, borderStyle, onPointerMove, styleIf, viewMaybe)
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
    ( Model (Selection.init placeholderPaces) Nothing Nothing
    , Cmd.none
    )


placeholderPaces =
    [ ( "Very Easy", "7:35" )
    , ( "Easy", "6:54" )
    , ( "Moderate", "6:38" )
    , ( "Steady State", "6:20" )
    , ( "Brisk", "6:08" )
    , ( "Aerobic Threshold", "5:53" )
    , ( "Lactate Threshold", "5:38" )
    , ( "Groove", "5:22" )
    , ( "VO2 Max", "5:07" )
    , ( "Fast", "4:52" )
    ]
        |> List.map
            (\( name, pace ) ->
                { name = Validate.init Ok name name
                , pace = Validate.init Validate.parsePace (7 * 60) pace |> Validate.updateFallback
                , yOffset = 0
                , dragOffset = 0
                , dragValue = pace
                }
                    |> updatePaceFormPosition 0
            )


type alias Model =
    { trainingPaces : Selection PaceForm
    , initialDragPosition : Maybe ( Float, Float )
    , error : Maybe String
    }


type alias PaceForm =
    { name : Validate.Field String String
    , pace : Validate.Field String Int
    , yOffset : Float
    , dragOffset : Float
    , dragValue : String
    }


type Msg
    = ClickedSave
    | PostedPaces (Result Http.Error Bool)
    | EditedPace Int String
    | EditedName Int String
    | ClickedAddPace
    | ClickedRemovePace Int
    | ClickedDragPace Int Int Float Float
    | BlurredPace
    | PointerMoved Float Float
    | PointerUp Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave ->
            let
                paces =
                    Selection.toList model.trainingPaces
                        |> List.map (\{ name, pace } -> ( pace.fallback, name.fallback ))
            in
            ( model
            , Task.attempt PostedPaces (Api.postTrainingPaces paces)
            )

        PostedPaces result ->
            ( case result of
                Err error ->
                    { model | error = Just (Api.errorString error) }

                _ ->
                    { model | error = Nothing }
            , Cmd.none
            )

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

        ClickedDragPace index pointerId pageX pageY ->
            ( { model
                | initialDragPosition = Just ( pageX, pageY )
                , trainingPaces = Selection.select index model.trainingPaces
              }
            , Ports.setPointerCapture { targetId = config.trainingPaceListId, pointerId = pointerId }
            )

        PointerMoved _ y ->
            let
                updateForm form =
                    Maybe.map (\( _, iy ) -> y - iy) model.initialDragPosition
                        |> Maybe.map (\dragOffset -> updatePaceFormPosition dragOffset form)
                        |> Maybe.withDefault form
            in
            ( { model | trainingPaces = Selection.update updateForm model.trainingPaces }
            , Cmd.none
            )

        PointerUp y ->
            ( { model
                | initialDragPosition = Nothing
                , trainingPaces = Selection.update (\form -> { form | pace = Validate.update form.dragValue form.pace |> Validate.updateFallback } |> updatePaceFormPosition 0) model.trainingPaces
              }
            , Cmd.none
            )

        BlurredPace ->
            ( { model
                | trainingPaces = Selection.update (\form -> { form | pace = Validate.updateFallback form.pace } |> updatePaceFormPosition 0) model.trainingPaces
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


newTrainingPace : PaceForm
newTrainingPace =
    { name = Validate.init Ok "" ""
    , pace = Validate.init Validate.parsePace config.maxPace ""
    , yOffset = 0
    , dragOffset = 0
    , dragValue = ""
    }
        |> updatePaceFormPosition 0


updatePaceFormPosition : Float -> PaceForm -> PaceForm
updatePaceFormPosition dragOffset form =
    let
        { minPace, maxPace, sliderHeight } =
            config

        yOffset =
            form.pace.fallback
                |> (\seconds -> (1 - (toFloat seconds - minPace) / (maxPace - minPace)) * sliderHeight)

        clampedDragOffset =
            dragOffset
                |> clamp -yOffset (sliderHeight - yOffset)

        draggedPace =
            (-clampedDragOffset / sliderHeight)
                * (maxPace - minPace)
                |> round
                |> (+) form.pace.fallback
                |> Pace.paceToString
    in
    { form | yOffset = yOffset, dragOffset = clampedDragOffset, dragValue = draggedPace }


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
viewBody { trainingPaces, initialDragPosition, error } =
    let
        dragActive =
            initialDragPosition /= Nothing
    in
    column [ style "margin" "5px" ]
        [ row []
            [ compactColumn []
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [ style "margin-bottom" "0.5rem", style "margin-right" "10px" ] [ Html.text "Training Paces" ]
                    , viewAddButton
                    , column [] []
                    , viewSaveButton
                    ]
                , row [ style "height" "1rem", style "justify-content" "flex-end", style "color" "var(--red-700)" ] [ viewMaybe error text ]
                , row []
                    [ viewTrainingPaces (initialDragPosition /= Nothing) (Selection.selectedIndex trainingPaces) (Selection.toList trainingPaces)
                    , column [] []
                    ]
                ]
            ]
        ]


config =
    { sliderHeight = 600
    , maxPace = 8 * 60
    , minPace = 4 * 60
    , trainingPaceListId = "training-pace-list"
    }


viewTrainingPaces : Bool -> Int -> List PaceForm -> Html Msg
viewTrainingPaces dragActive selectedIndex paces =
    let
        paceTicks =
            List.range 4 8 |> List.reverse |> List.map ((*) 60)
    in
    compactColumn
        [ Html.Attributes.id config.trainingPaceListId
        , styleIf dragActive "touch-action" "none"
        , attributeIf dragActive (onPointerMove PointerMoved)
        , attributeIf dragActive (Html.Events.on "pointerup" (Decode.map PointerUp (Decode.field "y" Decode.float)))
        , style "height" (String.fromInt config.sliderHeight ++ "px")
        , style "position" "relative"
        , style "width" "310px"
        ]
        (List.indexedMap (viewPaceForm dragActive) paces ++ List.map viewPaceTick paceTicks)


viewPaceTick : Int -> Html Msg
viewPaceTick seconds =
    let
        { minPace, maxPace, sliderHeight } =
            config

        yOffset =
            (1 - (toFloat seconds - minPace) / (maxPace - minPace)) * sliderHeight
    in
    row
        [ style "margin-top" "5px"
        , style "padding-top" "2px"
        , style "padding-bottom" "3px"
        , style "margin-bottom" "5px"
        , style "position" "absolute"
        , style "z-index" "0"
        , style "top" (String.fromFloat yOffset ++ "px")
        , style "width" "100%"
        , borderStyle "border-bottom"
        , style "color" "var(--grey-900)"
        ]
        [ text (Pace.paceToString seconds) ]


viewPaceForm : Bool -> Int -> PaceForm -> Html Msg
viewPaceForm dragActive index { name, pace, yOffset, dragOffset, dragValue } =
    row
        [ style "margin-top" "5px"
        , style "margin-bottom" "5px"
        , style "margin-left" "5px"
        , style "position" "absolute"
        , styleIf dragActive "pointer-events" "none"
        , styleIf dragActive "touch-action" "none"
        , styleIf (dragOffset /= 0) "z-index" "2"
        , style "z-index" "1"
        , style "top" (String.fromFloat (yOffset + dragOffset) ++ "px")
        ]
        [ Button.action "Drag" MonoIcons.drag NoOp
            |> Button.withAttributes
                [ class "row__button--drag"
                , Html.Events.on "pointerdown"
                    (Decode.map3 (ClickedDragPace index)
                        (Decode.field "pointerId" Decode.int)
                        (Decode.field "pageX" Decode.float)
                        (Decode.field "pageY" Decode.float)
                    )
                ]
            |> Button.withAppearance Button.Tiny Button.Subtle Button.None
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
            |> UI.Input.withAppearance UI.Input.Tiny
            |> UI.Input.view
                (if dragActive && dragOffset /= 0 then
                    dragValue

                 else
                    pace.value
                )
        , compactColumn [ style "width" "5px" ] []
        , UI.Input.text (EditedName index)
            |> UI.Input.withResultError name.result
            |> UI.Input.withAppearance UI.Input.Tiny
            |> UI.Input.view name.value
        , Button.action "Remove Pace" MonoIcons.remove (ClickedRemovePace index)
            |> Button.withAppearance Button.Tiny Button.Subtle Button.Right
            |> Button.view
        ]


viewAddButton : Html Msg
viewAddButton =
    Button.action "Add Pace" MonoIcons.add ClickedAddPace
        |> Button.withAppearance Button.Small Button.Subtle Button.Right
        |> Button.view


viewSaveButton : Html Msg
viewSaveButton =
    Button.action "Save" MonoIcons.check ClickedSave
        |> Button.withAppearance Button.Large Button.Primary Button.Right
        |> Button.view
