module Page.Settings exposing (main)

import Activity
import Activity.Types exposing (RaceDistance)
import ActivityForm
import Api
import Browser
import Duration.View
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Http
import Json.Decode as Decode
import MPRLevel
import MonoIcons
import Pace
import Pace.List exposing (PaceList)
import Ports
import Selection exposing (Selection)
import Task
import UI
import UI.Button as Button
import UI.Input
import UI.Label
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Select
import UI.Skeleton as Skeleton
import UI.Util exposing (attributeIf, attributeMaybe, borderStyle, onPointerMove, styleIf, viewIf, viewMaybe)
import Validate exposing (Field)


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Settings | Rhino Log", body = [ view model ] }
        , update = \model msg -> update model msg
        , subscriptions = \_ -> Ports.setDropTarget SetDropTarget
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (initPaces []) Nothing Nothing initRaceDuration (Err "") Loading
    , Task.attempt GotSettings Api.getSettings
    )


initPaces paces =
    paces
        |> List.map
            (\( name, pace ) ->
                { name = Validate.init Ok name name
                , pace = Validate.init Validate.parsePace pace (Pace.paceToString pace)
                , dropTarget = False
                , ordered = True
                }
            )
        |> Selection.init


initRaceDuration =
    Validate.init Validate.parseDuration 0 ( "", "", "" )


type alias Model =
    { trainingPaces : Selection PaceForm
    , dragging : Maybe ( Float, Float )
    , raceDistance : Maybe RaceDistance
    , raceDuration : Validate.Field ( String, String, String ) Int
    , level : Result String Int
    , status : FormStatus
    }


type alias PaceForm =
    { name : Validate.Field String String
    , pace : Validate.Field String Int
    , dropTarget : Bool
    , ordered : Bool
    }


type FormStatus
    = Loading
    | Posted
    | Success
    | Error String


type Msg
    = ClickedSave
    | GotSettings (Result Http.Error (PaceList String))
    | PostedPaces (PaceList String) (Result Http.Error Bool)
    | EditedPace Int String
    | EditedName Int String
    | ClickedAddPace
    | ClickedRemovePace Int
    | ClickedDragPace Int Int
    | SetDropTarget Int
    | BlurredPace
    | PointerMoved Float Float
    | PointerUp
    | SelectedRaceDistance String
    | EditedDuration ( String, String, String )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave ->
            let
                paces =
                    Selection.toList model.trainingPaces
                        |> List.sortBy (\form -> form.pace.result |> Result.withDefault form.pace.fallback |> negate)
                        |> List.map (\{ name, pace } -> ( name.result |> Result.withDefault name.fallback, pace.result |> Result.withDefault pace.fallback ))
                        |> List.filter (\( name, pace ) -> name /= "")
            in
            ( { model | status = Posted }
            , Task.attempt (PostedPaces paces) (Api.postSettings paces)
            )

        GotSettings result ->
            ( case result of
                Err error ->
                    { model | status = Error (Api.errorString error) }

                Ok paces ->
                    { model | status = Success, trainingPaces = initPaces paces }
            , Cmd.none
            )

        PostedPaces paces result ->
            ( case result of
                Err error ->
                    { model | status = Error (Api.errorString error) }

                _ ->
                    { model | status = Success, trainingPaces = initPaces paces }
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

        ClickedDragPace index pointerId ->
            ( { model
                | trainingPaces =
                    Selection.select index model.trainingPaces
                , dragging = Just ( -100, -100 )
              }
            , Ports.setPointerCapture { targetId = config.trainingPaceListId, pointerId = pointerId }
            )

        SetDropTarget id ->
            ( { model
                | trainingPaces =
                    Selection.move id model.trainingPaces
                        |> Selection.update (\form -> { form | dropTarget = True })
              }
            , Cmd.none
            )

        PointerMoved x y ->
            ( { model | dragging = Just ( x, y ) }
            , Cmd.none
            )

        PointerUp ->
            ( { model
                | dragging = Nothing
                , trainingPaces = Selection.update (\form -> { form | dropTarget = False }) model.trainingPaces
              }
            , Cmd.none
            )

        BlurredPace ->
            if model.status == Success then
                ( { model | trainingPaces = Selection.update (\form -> { form | pace = Validate.updateFallback form.pace }) model.trainingPaces }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SelectedRaceDistance str ->
            ( { model | raceDistance = Activity.raceDistance.fromString str }
                |> updateLevel
            , Cmd.none
            )

        EditedDuration value ->
            ( { model | raceDuration = Validate.update value model.raceDuration }
                |> updateLevel
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateLevel : Model -> Model
updateLevel model =
    let
        levelM =
            Maybe.map Activity.raceDistance.toString model.raceDistance
                |> Result.fromMaybe "Please select a distance."
                |> Result.andThen
                    (\distance ->
                        model.raceDuration.result
                            |> Result.mapError (\_ -> "Please enter a valid time.")
                            |> Result.map (\duration -> ( distance, duration ))
                    )
                |> Result.andThen
                    (\( distance, duration ) ->
                        MPRLevel.lookup MPRLevel.Neutral distance duration
                            |> Result.map Tuple.second
                    )
    in
    { model | level = levelM }


newTrainingPace : PaceForm
newTrainingPace =
    { name = Validate.init Ok "" ""
    , pace = Validate.init Validate.parsePace config.maxPace ""
    , ordered = True
    , dropTarget = False
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
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Settings" ]
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


maxWidthForMobile =
    style "width" "310px"


viewBody : Model -> Html Msg
viewBody { trainingPaces, dragging, status, raceDistance, raceDuration, level } =
    column [ style "margin" "5px" ]
        [ row [ style "justify-content" "space-between", style "flex-wrap" "wrap-reverse" ]
            [ compactColumn [ maxWidthForMobile ]
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [ style "margin-bottom" "0.5rem", style "margin-right" "10px" ] [ Html.text "Training Paces" ]
                    ]
                , Html.text "Add custom paces to match your workouts and training plan."
                , row [ style "align-items" "center", style "margin-top" "10px", style "margin-bottom" "10px" ]
                    [ Html.a
                        [ Html.Attributes.href "https://www.rundoyen.com/running-pace-calculator/"
                        , Html.Attributes.target "_blank"
                        , style "display" "flex"
                        , style "align-items" "center"
                        ]
                        [ Html.text "Calculator"
                        , MonoIcons.icon (MonoIcons.externalLink "var(--blue-500)")
                        ]
                    ]
                , row []
                    [ column [ styleIf (status == Posted) "opacity" "0.5" ]
                        [ viewTrainingPaces dragging (Selection.selectedIndex trainingPaces) (Selection.toList trainingPaces)
                        , viewIf (status /= Loading) viewAddButton
                        ]
                    , viewMaybe dragging (\d -> viewDraggedPace d trainingPaces)
                    , column [] []
                    ]
                ]
            , compactColumn [ maxWidthForMobile ]
                [ Html.h3 [ style "margin-bottom" "0.5rem", style "margin-right" "10px" ] [ Html.text "Strava Account" ]
                , Html.text "Sign in with Strava to import your activities."
                , Html.div [ style "color" "var(--green-900)", style "margin-top" "0.5rem" ] [ Html.text "Connected ✓" ]
                , Html.h3 [ style "margin-bottom" "0.5rem", style "margin-right" "10px" ] [ Html.text "Fitness Level" ]
                , Html.text "Enter a recent race time to calculate your fitness level."
                , viewRecentRaceInput raceDuration raceDistance
                , viewLevelResult level
                ]
            , compactColumn []
                [ viewSaveButton status
                , viewStatusMessage status
                ]
            ]
        ]


config =
    { sliderHeight = 600
    , maxPace = 8 * 60
    , minPace = 4 * 60
    , trainingPaceListId = "training-pace-list"
    }


viewStatusMessage : FormStatus -> Html Msg
viewStatusMessage status =
    row [ style "height" "1rem", style "justify-content" "flex-end", style "color" "var(--red-700)" ]
        (case status of
            Error string ->
                [ text string ]

            _ ->
                []
        )


viewSaveButton : FormStatus -> Html Msg
viewSaveButton status =
    row [ style "justify-content" "center", style "margin-top" "1rem", style "min-width" "6rem" ]
        [ case status of
            Posted ->
                UI.spinner "2rem"

            Loading ->
                UI.spinner "2rem"

            _ ->
                Button.action "Save" MonoIcons.check ClickedSave
                    |> Button.withAppearance Button.Large Button.Primary Button.Bottom
                    |> Button.view
        ]


viewTrainingPaces : Maybe ( Float, Float ) -> Int -> List PaceForm -> Html Msg
viewTrainingPaces dragging selectedIndex paces =
    Html.node "list-dnd"
        [ Html.Attributes.id config.trainingPaceListId
        , attributeMaybe dragging (\_ -> onPointerMove PointerMoved)
        , attributeMaybe dragging (\_ -> Html.Events.on "pointerup" (Decode.succeed PointerUp))
        ]
        (List.indexedMap viewPaceForm paces)


viewPaceForm : Int -> PaceForm -> Html Msg
viewPaceForm index { name, pace, dropTarget, ordered } =
    row
        [ style "margin-top" "5px"
        , style "margin-bottom" "5px"
        , class "drop-target"
        , styleIf dropTarget "visibility" "hidden"
        , Html.Attributes.attribute "data-drop-id" (String.fromInt index)
        ]
        [ Button.action "Drag" MonoIcons.drag NoOp
            |> Button.withAttributes
                [ class "row__button--drag"
                , Html.Events.on "pointerdown" (Decode.map (ClickedDragPace index) (Decode.field "pointerId" Decode.int))
                ]
            |> Button.withAppearance Button.Small Button.Subtle Button.None
            |> Button.view
        , UI.Input.text (EditedName index)
            |> UI.Input.withResultError name.result
            |> UI.Input.view name.value
        , compactColumn [ style "width" "10px" ] []
        , UI.Input.pace (EditedPace index)
            |> (\input ->
                    case ( pace.result, ordered ) of
                        ( Err Validate.MissingError, _ ) ->
                            input

                        ( Err err, _ ) ->
                            UI.Input.withError err input

                        ( Ok _, False ) ->
                            UI.Input.withError Validate.ValueError input

                        _ ->
                            input
               )
            |> UI.Input.withPlaceholder (Result.map Pace.paceToString pace.result |> Result.withDefault "mm:ss")
            |> UI.Input.withAttributes [ Html.Events.onBlur BlurredPace ]
            |> UI.Input.view pace.value
        , Button.action "Remove Pace" MonoIcons.remove (ClickedRemovePace index)
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]


viewDraggedPace : ( Float, Float ) -> Selection PaceForm -> Html Msg
viewDraggedPace ( x, y ) trainingPaces =
    let
        paceForm =
            viewMaybe
                (Selection.get trainingPaces)
                (\pace ->
                    viewPaceForm (Selection.selectedIndex trainingPaces) { pace | dropTarget = False }
                )
    in
    Html.div
        [ style "position" "absolute"
        , style "left" (String.fromFloat (x - 20) ++ "px")
        , style "top" (String.fromFloat (y - 20) ++ "px")
        , style "opacity" "0.5"
        , style "touch-action" "none"
        , Html.Attributes.id "dragged-element"
        ]
        [ paceForm
        ]


viewAddButton : Html Msg
viewAddButton =
    row [ style "justify-content" "flex-end" ]
        [ Button.action "Add Pace" MonoIcons.add ClickedAddPace
            |> Button.withAppearance Button.Small Button.Subtle Button.Right
            |> Button.view
        ]


viewLevelResult : Result String Int -> Html msg
viewLevelResult level =
    let
        ( color, str ) =
            case level of
                Ok num ->
                    ( "var(--green-900)", "Level " ++ String.fromInt num )

                Err err ->
                    ( "var(--orange-500)", err )
    in
    row [ style "color" color, style "margin-top" "0.5rem", style "margin-bottom" "0.5rem", style "height" "1rem" ] [ Html.text str ]


viewRecentRaceInput : Validate.Field ( String, String, String ) Int -> Maybe RaceDistance -> Html Msg
viewRecentRaceInput raceDuration raceDistance =
    let
        ( hrs, mins, secs ) =
            raceDuration.value
    in
    row [ style "margin-top" "10px" ]
        [ Duration.View.input EditedDuration ( hrs, mins, secs )
        , compactColumn [ style "width" "10px" ] []
        , column []
            [ UI.Label.input "DISTANCE" |> UI.Label.view
            , UI.Select.select SelectedRaceDistance ("" :: (Activity.raceDistance.list |> List.map Tuple.first))
                |> UI.Select.view (raceDistance |> Maybe.map Activity.raceDistance.toString |> Maybe.withDefault "")
            ]
        ]
