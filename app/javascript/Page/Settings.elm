module Page.Settings exposing (main)

import Activity
import Activity.Types exposing (RaceDistance)
import Api
import App
import Browser
import Browser.Navigation
import Duration
import Duration.View
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import MPRLevel
import MonoIcons
import Pace
import Ports
import Report
import Selection exposing (Selection)
import Settings exposing (Settings)
import Task
import UI
import UI.Button as Button
import UI.Input
import UI.Label
import UI.Layout exposing (column, compactColumn, row)
import UI.Navbar as Navbar
import UI.Select
import UI.Skeleton as Skeleton
import UI.Toast
import UI.Util exposing (attributeMaybe, borderStyle, onPointerMove, styleIf, viewIf, viewMaybe)
import Validate exposing (Field)


main : Program App.Flags ( App.Env, Model ) Msg
main =
    App.document
        { title = "Settings"
        , init = init
        , update = update
        , perform = identity
        , view = view
        , subscriptions = \_ -> Ports.setDropTarget SetDropTarget
        }


init : App.Env -> ( Model, Cmd Msg )
init env =
    ( Model (initPaces []) Nothing Nothing (initRaceDuration Nothing) (Err "") Loading (Err "")
    , Task.attempt GotSettings Api.getSettings
    )


initPaces : List ( a, Int ) -> Selection { name : Field a a, pace : Field String Int, dropTarget : Bool, ordered : Bool }
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


initRaceDuration : Maybe Int -> Field ( String, String, String ) Int
initRaceDuration secsM =
    case secsM of
        Just secs ->
            Validate.init Validate.parseDuration secs (Duration.toHrsMinsSecs secs |> (\( h, m, s ) -> ( String.fromInt h, String.fromInt m, String.fromInt s )))

        Nothing ->
            Validate.init Validate.parseDuration 0 ( "", "", "" )


type alias Model =
    { trainingPaces : Selection PaceForm
    , dragging : Maybe ( Float, Float )
    , raceDistance : Maybe RaceDistance
    , raceDuration : Validate.Field ( String, String, String ) Int
    , level : Result String Int
    , status : FormStatus
    , result : Result String Settings
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
    = ClickedSave Settings
    | GotSettings (Result Http.Error (Maybe Settings))
    | PostedSettings Settings (Result Http.Error Bool)
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


update : App.Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case msg of
        ClickedSave settings ->
            ( { model | status = Posted }
            , Task.attempt (PostedSettings settings) (Api.putSettings settings)
            )

        GotSettings result ->
            case result of
                Err err ->
                    ( { model | status = Error strings.loadError }
                    , reportError env "GotSettings" (Api.developerError err)
                    )

                Ok (Just settings) ->
                    ( { model
                        | status = Success
                        , trainingPaces = initPaces settings.paces
                        , raceDistance = Just settings.raceDistance
                        , raceDuration = initRaceDuration (Just settings.raceDuration)
                      }
                        |> updateLevel
                        |> updateResult
                    , Cmd.none
                    )

                Ok Nothing ->
                    ( { model | status = Success }
                    , Cmd.none
                    )

        PostedSettings settings result ->
            case result of
                Err error ->
                    ( { model | status = Error (Api.userError error) }
                    , reportError env "PostedSettings" (Api.developerError error)
                    )

                _ ->
                    ( { model | status = Success, trainingPaces = initPaces settings.paces }
                    , Browser.Navigation.load "/calendar"
                    )

        EditedPace index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (\form -> { form | pace = Validate.update str form.pace })
            in
            ( { model | trainingPaces = newTrainingPaces }
                |> updateResult
            , Cmd.none
            )

        EditedName index str ->
            let
                newTrainingPaces =
                    Selection.select index model.trainingPaces
                        |> Selection.update (\form -> { form | name = Validate.update str form.name })
            in
            ( { model | trainingPaces = newTrainingPaces }
                |> updateResult
            , Cmd.none
            )

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
                |> updateResult
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
                |> updateResult
            , Cmd.none
            )

        BlurredPace ->
            if model.status == Success then
                ( { model | trainingPaces = Selection.update (\form -> { form | pace = Validate.updateFallback form.pace }) model.trainingPaces }
                    |> updateResult
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SelectedRaceDistance str ->
            ( { model | raceDistance = Activity.raceDistance.fromString str }
                |> updateLevel
                |> updateResult
            , Cmd.none
            )

        EditedDuration value ->
            ( { model | raceDuration = Validate.update value model.raceDuration }
                |> updateLevel
                |> updateResult
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateLevel : Model -> Model
updateLevel model =
    let
        levelM =
            Maybe.map Activity.raceDistance.toString model.raceDistance
                |> Result.fromMaybe strings.invalidDistanceError
                |> Result.andThen
                    (\distance ->
                        model.raceDuration.result
                            |> Result.mapError
                                (\err ->
                                    if err == Validate.MissingError then
                                        strings.invalidTimeError

                                    else
                                        ""
                                )
                            |> Result.map (\duration -> ( distance, duration ))
                    )
                |> Result.andThen
                    (\( distance, duration ) ->
                        MPRLevel.lookup MPRLevel.Neutral distance duration
                            |> Result.map Tuple.second
                            |> Result.mapError (\_ -> strings.invalidTimeError)
                    )
    in
    { model | level = levelM }


updateResult : Model -> Model
updateResult model =
    let
        paces =
            Selection.toList model.trainingPaces
                |> List.sortBy (\form -> form.pace.result |> Result.withDefault form.pace.fallback |> negate)
                |> List.map (\{ name, pace } -> ( name.result |> Result.withDefault name.fallback, pace.result |> Result.withDefault pace.fallback ))
                |> List.filter (\( name, _ ) -> name /= "")

        withMissingRaceError =
            Result.mapError (\_ -> strings.missingRaceError)

        result =
            Result.map3
                (\distance duration level ->
                    { paces = paces
                    , raceDistance = distance
                    , raceDuration = duration
                    , level = level
                    }
                )
                (model.raceDistance |> Result.fromMaybe "" |> withMissingRaceError)
                (model.raceDuration.result |> withMissingRaceError)
                (model.level |> withMissingRaceError)
    in
    { model | result = result }


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
        navHeader =
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Settings" ]
    in
    Skeleton.default
        |> Skeleton.withTitle "Settings"
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withItems [ navHeader ]
                |> Navbar.withRightItem (viewSaveButton model.status model.result)
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html Msg
viewBody { trainingPaces, dragging, status, raceDistance, raceDuration, level, result } =
    column [ style "margin" "15px", style "margin-top" "40px", style "margin-bottom" "40px" ]
        [ viewStatusMessage status result
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ Html.h3 [] [ Html.text "Recent Race" ]
                , Html.text "Enter a recent race time to calculate your fitness level."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ row [] [ viewRecentRaceInput raceDuration raceDistance ]
                , viewLevelResult level
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [] [ Html.text "Standard Paces" ]
                    ]
                , Html.text "These paces will be used to adjust your log to your current fitness level."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ viewStandardPaces level
                ]
            ]
        , Html.hr [ class "hr--spacer" ] []
        , row [ style "flex-wrap" "wrap" ]
            [ compactColumn [ class "column--mobile" ]
                [ row [ style "align-items" "flex-end" ]
                    [ Html.h3 [] [ Html.text "Custom Paces" ]
                    ]
                , Html.text "Add additional paces used in your workouts and training plan."
                ]
            , column [ class "column--spacer" ] []
            , compactColumn [ class "column--mobile" ]
                [ row [ style "margin-top" "10px", style "margin-bottom" "30px" ]
                    [ column [ styleIf (status == Posted) "opacity" "0.5", style "margin-left" "-10px" ]
                        [ viewTrainingPaces dragging (Selection.selectedIndex trainingPaces) (Selection.toList trainingPaces)
                        , viewIf (status /= Loading) viewAddButton
                        , viewMaybe dragging (\d -> viewDraggedPace d trainingPaces)
                        ]
                    ]
                ]
            ]
        ]


config : { maxPace : number, trainingPaceListId : String }
config =
    { maxPace = 8 * 60
    , trainingPaceListId = "training-pace-list"
    }


strings : { loadError : String, invalidTimeError : String, invalidDistanceError : String, missingRaceError : String }
strings =
    { loadError = "There was an issue loading your settings, please try again."
    , invalidTimeError = "Please enter a valid time."
    , invalidDistanceError = "Please select a distance."
    , missingRaceError = "Please enter a recent race."
    }


viewStatusMessage : FormStatus -> Result String Settings -> Html Msg
viewStatusMessage status result =
    let
        viewMessage str =
            if str /= "" then
                UI.Toast.top
                    |> UI.Toast.view (text str)
                    |> viewDelayedMessage str

            else
                text ""
    in
    case ( status, result ) of
        ( Error string, _ ) ->
            viewMessage string

        ( _, Err string ) ->
            viewMessage string

        _ ->
            text ""


viewDelayedMessage : String -> Html msg -> Html msg
viewDelayedMessage key content =
    Html.Keyed.node "div"
        []
        [ ( key
          , Html.div
                [ style "animation" "appear 0.5s ease 1s", style "animation-fill-mode" "backwards" ]
                [ content ]
          )
        ]


viewSaveButton : FormStatus -> Result String Settings -> Html Msg
viewSaveButton status result =
    compactColumn [ style "min-width" "6rem", style "align-items" "flex-end" ]
        [ case ( status, result ) of
            ( Posted, _ ) ->
                UI.spinner "2rem"

            ( Loading, _ ) ->
                UI.spinner "2rem"

            ( _, Ok settings ) ->
                Button.action "Save" MonoIcons.check (ClickedSave settings)
                    |> Button.withAppearance Button.Wide Button.Primary Button.Bottom
                    |> Button.view

            ( _, Err _ ) ->
                Button.action "Save" MonoIcons.check NoOp
                    |> Button.withAppearance Button.Wide Button.Subtle Button.Bottom
                    |> Button.view
        ]


viewStandardPaces : Result String Int -> Html msg
viewStandardPaces levelR =
    let
        paces =
            Result.map
                (\level ->
                    Pace.standardPaces ( MPRLevel.Neutral, level )
                        |> List.map Tuple.second
                        |> List.map Pace.paceToString
                )
                levelR
                |> Result.withDefault (List.repeat (List.length Pace.standardPace.list) "")
    in
    row [ style "margin-top" "10px", style "margin-bottom" "10px" ]
        [ compactColumn []
            (List.map2
                (\name pace ->
                    row [ style "margin-top" "8px", style "padding-bottom" "5px", borderStyle "border-bottom", style "min-width" "200px" ]
                        [ column [] [ text name ]
                        , column [ style "align-items" "flex-end" ] [ text pace ]
                        ]
                )
                (Pace.standardPace.list |> List.map Tuple.first)
                paces
            )
        ]


viewTrainingPaces : Maybe ( Float, Float ) -> Int -> List PaceForm -> Html Msg
viewTrainingPaces dragging _ paces =
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
        , style "align-items" "center"
        , Html.Attributes.attribute "data-drop-id" (String.fromInt index)
        ]
        [ Button.action "Drag" MonoIcons.drag NoOp
            |> Button.withAttributes
                [ class "row__button--drag"
                , Html.Events.on "pointerdown" (Decode.map (ClickedDragPace index) (Decode.field "pointerId" Decode.int))
                ]
            |> Button.withAppearance Button.Tiny Button.Subtle Button.None
            |> Button.view
        , UI.Input.text (EditedName index)
            |> UI.Input.withResultError name.result
            |> UI.Input.view name.value
        , compactColumn [ style "width" "5px" ] []
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
        [ style "position" "fixed"
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
    row []
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
    viewDelayedMessage str
        (Html.div [ style "color" color, style "margin-top" "5px", style "height" "1rem" ] [ Html.text str ])


viewRecentRaceInput : Validate.Field ( String, String, String ) Int -> Maybe RaceDistance -> Html Msg
viewRecentRaceInput raceDuration raceDistance =
    let
        ( hrs, mins, secs ) =
            raceDuration.value
    in
    column [ style "margin-top" "10px" ]
        [ row []
            [ compactColumn []
                [ UI.Label.input "DISTANCE" |> UI.Label.view
                , UI.Select.select SelectedRaceDistance ("" :: (Activity.raceDistance.list |> List.filter (\( _, dist ) -> dist /= Activity.Types.OtherDistance) |> List.map Tuple.first))
                    |> UI.Select.view (raceDistance |> Maybe.map Activity.raceDistance.toString |> Maybe.withDefault "")
                ]
            ]
        , row [ style "margin-top" "10px" ] [ Duration.View.input EditedDuration ( hrs, mins, secs ) ]
        ]



-- ERROR REPORTING


reportError : App.Env -> String -> String -> Cmd Msg
reportError env msg errorMsg =
    Report.error env
        |> Report.withField "msg" (Encode.string msg)
        |> Report.send errorMsg
        |> Task.attempt (\_ -> NoOp)
