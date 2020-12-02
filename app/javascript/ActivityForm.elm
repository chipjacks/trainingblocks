module ActivityForm exposing (init, initMove, isEditing, update, view)

import Activity exposing (Activity, ActivityData, Minutes)
import ActivityShape
import Api
import Array exposing (Array)
import Date exposing (Date)
import Emoji
import Html exposing (Html, a, button, div, i, input, span, text)
import Html.Attributes exposing (class, href, id, name, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Http
import Json.Decode as Decode
import MPRLevel exposing (stripTimeStr)
import Msg exposing (ActivityForm, ActivityState(..), DataForm(..), FormError(..), Msg(..))
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, row, viewIf, viewMaybe)
import Store
import Task exposing (Task)



{- GOOD EXAMPLE:
   https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Settings.elm
-}


init : Activity -> ActivityForm
init activity =
    let
        baseModel =
            ActivityForm activity.id (Just activity.date) activity.description (Ok activity)
    in
    baseModel <|
        case activity.data of
            Activity.Run minutes pace_ completed ->
                RunForm { duration = String.fromInt minutes, pace = pace_, completed = completed }

            Activity.Interval seconds pace_ completed ->
                IntervalForm { duration = String.fromInt seconds, pace = pace_, completed = completed }

            Activity.Race minutes distance_ completed ->
                RaceForm { duration = String.fromInt minutes, distance = distance_, completed = completed }

            Activity.Other minutes completed ->
                OtherForm { duration = String.fromInt minutes, completed = completed }

            Activity.Note emoji_ ->
                NoteForm { emoji = emoji_ }

            Activity.Session activities ->
                SessionForm activities


initMove : Activity -> ActivityForm
initMove activity =
    let
        model =
            init activity
    in
    { model | date = Nothing }


apply : (Activity -> Msg) -> ActivityForm -> Msg
apply toMsg { result } =
    case result of
        Ok activity ->
            toMsg activity

        _ ->
            NoOp


isEditing : Activity -> ActivityForm -> Bool
isEditing activity { id } =
    activity.id == id


validateFieldExists : Maybe a -> String -> Result FormError a
validateFieldExists fieldM fieldName =
    case fieldM of
        Just field ->
            Ok field

        Nothing ->
            Err <| EmptyFieldError fieldName


validate : ActivityForm -> Result FormError Activity
validate model =
    Result.map2
        (\date description ->
            Activity
                model.id
                date
                description
                (toActivityData model.dataForm)
        )
        (validateFieldExists model.date "date")
        (validateFieldExists (Just model.description) "description")


update : Msg -> ActivityForm -> ( ActivityForm, Cmd Msg )
update msg model =
    case msg of
        SelectedDate date ->
            case model.date of
                Nothing ->
                    let
                        newModel =
                            updateResult { model | date = Just date }
                    in
                    ( newModel, Store.cmd (apply (Move date) newModel) )

                _ ->
                    ( model, Cmd.none )

        SelectedShape activityData ->
            case activityData of
                Activity.Run mins pace_ completed ->
                    ( updateResult
                        { model
                            | dataForm = RunForm { duration = String.fromInt mins, pace = pace_, completed = completed }
                        }
                    , Cmd.none
                    )

                Activity.Interval secs pace_ completed ->
                    ( updateResult
                        { model
                            | dataForm = IntervalForm { duration = String.fromInt secs, pace = pace_, completed = completed }
                        }
                    , Cmd.none
                    )

                Activity.Race mins dist completed ->
                    ( updateResult
                        { model
                            | dataForm = RaceForm { duration = String.fromInt mins, distance = dist, completed = completed }
                        }
                    , Cmd.none
                    )

                Activity.Other mins completed ->
                    ( updateResult
                        { model
                            | dataForm = OtherForm { duration = String.fromInt mins, completed = completed }
                        }
                    , Cmd.none
                    )

                Activity.Note emoji ->
                    ( updateResult
                        { model
                            | dataForm = NoteForm { emoji = emoji }
                        }
                    , Cmd.none
                    )

                Activity.Session activities ->
                    ( updateResult
                        { model
                            | dataForm = SessionForm activities
                        }
                    , Cmd.none
                    )

        EditedDescription desc ->
            ( updateResult { model | description = desc }
            , Cmd.none
            )

        SelectedEmoji char ->
            ( updateResult { model | dataForm = NoteForm { emoji = char } }
            , Cmd.none
            )

        CheckedCompleted ->
            ( updateResult { model | dataForm = updateCompleted model.dataForm }
            , Cmd.none
            )

        EditedDuration str ->
            ( updateResult { model | dataForm = updateDuration str model.dataForm }
            , Cmd.none
            )

        SelectedPace str ->
            ( updateResult { model | dataForm = updatePace str model.dataForm }
            , Cmd.none
            )

        SelectedDistance str ->
            ( updateResult { model | dataForm = updateDistance str model.dataForm }
            , Cmd.none
            )

        ClickedSubmit ->
            ( model, Store.cmd (apply Update model) )

        _ ->
            ( model, Cmd.none )


updateCompleted : DataForm -> DataForm
updateCompleted dataForm =
    case dataForm of
        RunForm data ->
            RunForm { data | completed = not data.completed }

        IntervalForm data ->
            IntervalForm { data | completed = not data.completed }

        RaceForm data ->
            RaceForm { data | completed = not data.completed }

        OtherForm data ->
            OtherForm { data | completed = not data.completed }

        _ ->
            dataForm


updateDuration : String -> DataForm -> DataForm
updateDuration duration dataForm =
    case dataForm of
        RunForm data ->
            RunForm { data | duration = duration }

        IntervalForm data ->
            IntervalForm { data | duration = duration }

        RaceForm data ->
            RaceForm { data | duration = duration }

        OtherForm data ->
            OtherForm { data | duration = duration }

        _ ->
            dataForm


updatePace : String -> DataForm -> DataForm
updatePace paceStr dataForm =
    case dataForm of
        RunForm data ->
            RunForm { data | pace = Activity.pace.fromString paceStr |> Maybe.withDefault (defaults dataForm |> .pace) }

        IntervalForm data ->
            IntervalForm { data | pace = Activity.pace.fromString paceStr |> Maybe.withDefault (defaults dataForm |> .pace) }

        _ ->
            dataForm


updateDistance : String -> DataForm -> DataForm
updateDistance distanceStr dataForm =
    case dataForm of
        RaceForm data ->
            RaceForm { data | distance = Activity.distance.fromString distanceStr |> Maybe.withDefault (defaults dataForm |> .distance) }

        _ ->
            dataForm


updateResult : ActivityForm -> ActivityForm
updateResult model =
    { model | result = validate model }


view : Maybe Int -> ActivityState -> Html Msg
view levelM activityM =
    let
        dataInputs form result =
            case form of
                RunForm { duration, pace } ->
                    [ compactColumn [] [ durationInput EditedDuration False duration ]
                    , compactColumn [] [ paceSelect levelM SelectedPace pace ]
                    ]

                IntervalForm { duration, pace } ->
                    [ compactColumn [] [ durationInput EditedDuration True duration ]
                    , compactColumn [] [ paceSelect levelM SelectedPace pace ]
                    ]

                RaceForm { duration, distance } ->
                    [ compactColumn [] [ durationInput EditedDuration False duration ]
                    , compactColumn [] [ distanceSelect SelectedDistance distance ]
                    , compactColumn []
                        [ viewMaybe (Result.toMaybe result |> Maybe.andThen Activity.mprLevel)
                            (\level -> text <| "Level " ++ String.fromInt level)
                        ]
                    ]

                OtherForm { duration } ->
                    [ compactColumn [] [ durationInput EditedDuration False duration ] ]

                NoteForm { emoji } ->
                    [ compactColumn [] [ emojiSelect SelectedEmoji emoji ] ]

                SessionForm _ ->
                    []

        sharedAttributes =
            [ borderStyle "border-bottom"
            , style "position" "absolute"
            , style "left" "0"
            , style "right" "0"
            , style "background-color" "white"
            , style "z-index" "2"
            ]

        openAttributes minHeight maxHeight =
            [ style "transition" "max-height 0.5s, min-height 0.5s"
            , style "max-height" maxHeight
            , style "min-height" minHeight
            , style "padding" "0.5rem 0.5rem"
            , style "border-width" "1px"
            ]
                ++ sharedAttributes

        closedAttributes =
            [ style "transition" "max-height 0.5s, min-height 0.5s, border-width 0.5s 0.1s"
            , style "min-height" "0"
            , style "max-height" "0"
            , style "border-width" "0"
            ]
                ++ sharedAttributes
    in
    case activityM of
        Selected [ activity ] ->
            row (openAttributes "1rem" "2rem")
                [ column []
                    [ viewButtons activity False ]
                ]

        Selected activities ->
            row (openAttributes "1rem" "2rem")
                [ column []
                    [ viewMultiSelectButtons activities ]
                ]

        Editing model ->
            row (openAttributes "5rem" "20rem")
                [ column []
                    [ row []
                        [ viewMaybe (Result.toMaybe model.result)
                            (\activity ->
                                column [ style "margin-bottom" "1rem" ]
                                    [ viewButtons activity True ]
                            )
                        ]
                    , row []
                        [ viewShape model
                        , column []
                            [ row []
                                [ text (Maybe.map (Date.format "E MMM d") model.date |> Maybe.withDefault "Select Date")
                                ]
                            , row []
                                [ input
                                    [ type_ "text"
                                    , Html.Attributes.autocomplete False
                                    , placeholder "Description"
                                    , onInput EditedDescription
                                    , name "description"
                                    , value model.description
                                    , style "width" "100%"
                                    ]
                                    []
                                ]
                            , row [ style "flex-wrap" "wrap", style "align-items" "center" ] <|
                                compactColumn [ style "margin-right" "0.2rem" ] [ shapeSelect model ]
                                    :: dataInputs model.dataForm model.result
                            , row []
                                [ viewError model.result ]
                            ]
                        ]
                    ]
                ]

        _ ->
            row closedAttributes []


viewButtons : Activity -> Bool -> Html Msg
viewButtons activity editing =
    row []
        [ if editing then
            toolbarButton ClickedSubmit "mi-check" True

          else
            toolbarButton (EditActivity activity) "mi-edit" False
        , toolbarButton (ClickedCopy activity) "mi-copy" False
        , toolbarButton (Delete activity) "mi-delete" False
        , column [] []
        , toolbarButton (ClickedMove activity) "mi-calendar" False
        , toolbarButton (Shift True activity) "mi-arrow-up" False
        , toolbarButton (Shift False activity) "mi-arrow-down" False
        , column [] []
        , case activity.data of
            Activity.Session activities ->
                toolbarButton (Ungroup activities activity) "mi-folder" False

            _ ->
                Html.text ""
        , column [] []
        , toolbarButton ClickedClose "mi-close" False
        ]


toolbarButton : Msg -> String -> Bool -> Html Msg
toolbarButton onClickMsg iconClass primary =
    a
        [ class "button small expand"
        , attributeIf primary (class "primary")
        , style "margin-right" "0.2rem"
        , style "text-align" "center"
        , style "max-width" "3rem"
        , onClick onClickMsg
        ]
        [ i [ class iconClass ] [] ]


viewMultiSelectButtons : List Activity -> Html Msg
viewMultiSelectButtons activities =
    row []
        [ toolbarButton ClickedGroup "mi-folder" False
        ]


viewShape : ActivityForm -> Html Msg
viewShape model =
    let
        activityShape =
            validate model
                |> Result.toMaybe
                |> Maybe.map ActivityShape.view
                |> Maybe.withDefault (ActivityShape.viewDefault True (Activity.Run 30 Activity.Easy True))
    in
    compactColumn
        [ class "dynamic-shape"
        , style "flex-basis" "3.3rem"
        , style "justify-content" "center"
        , Html.Events.onClick CheckedCompleted
        ]
        [ activityShape ]


shapeSelect : ActivityForm -> Html Msg
shapeSelect model =
    let
        { duration, pace, distance, emoji, completed } =
            defaults model.dataForm

        types =
            [ Activity.Run duration pace completed
            , Activity.Interval duration pace completed
            , Activity.Race duration distance completed
            , Activity.Other duration completed
            , Activity.Note emoji
            ]

        typeStr =
            toActivityData model.dataForm |> Activity.activityTypeToString
    in
    div [ class "dropdown medium" ]
        [ button [ class "button medium" ]
            [ text typeStr ]
        , viewIf (typeStr /= "Session")
            (div [ class "dropdown-content" ]
                (List.map
                    (\aType ->
                        a [ onClick (SelectedShape aType) ] [ row [] [ ActivityShape.viewDefault True aType, compactColumn [ style "margin-left" "0.5rem", style "margin-top" "0.1rem" ] [ text (Activity.activityTypeToString aType) ] ] ]
                    )
                    types
                )
            )
        ]


type alias Defaults =
    { duration : Int, pace : Activity.Pace, distance : Activity.Distance, completed : Bool, emoji : String }


defaults : DataForm -> Defaults
defaults dataForm =
    let
        duration_ =
            parseDuration <|
                case dataForm of
                    RunForm { duration } ->
                        duration

                    RaceForm { duration } ->
                        duration

                    OtherForm { duration } ->
                        duration

                    _ ->
                        "30"

        pace_ =
            case dataForm of
                RunForm { pace } ->
                    pace

                _ ->
                    Activity.Easy

        distance_ =
            case dataForm of
                RaceForm { distance } ->
                    distance

                _ ->
                    Activity.FiveK

        completed_ =
            case dataForm of
                RunForm { completed } ->
                    completed

                RaceForm { completed } ->
                    completed

                OtherForm { completed } ->
                    completed

                _ ->
                    True

        emoji_ =
            case dataForm of
                NoteForm { emoji } ->
                    emoji

                _ ->
                    Emoji.default.name
    in
    Defaults duration_ pace_ distance_ completed_ emoji_


parseDuration : String -> Int
parseDuration str =
    if String.isEmpty str then
        0

    else
        String.toInt str |> Maybe.withDefault 0


toActivityData : DataForm -> ActivityData
toActivityData dataForm =
    case dataForm of
        RunForm { duration, pace, completed } ->
            Activity.Run (parseDuration duration) pace completed

        IntervalForm { duration, pace, completed } ->
            Activity.Interval (parseDuration duration) pace completed

        RaceForm { duration, distance, completed } ->
            Activity.Race (parseDuration duration) distance completed

        OtherForm { duration, completed } ->
            Activity.Other (parseDuration duration) completed

        NoteForm { emoji } ->
            Activity.Note emoji

        SessionForm activities ->
            Activity.Session activities


emojiSelect : (String -> Msg) -> String -> Html Msg
emojiSelect msg emoji =
    let
        emojis =
            Emoji.filter (String.toLower emoji) |> List.take 10

        padding =
            style "padding" "3.5px 0.5rem 0.5px 0.5rem"

        emojiItem data =
            a [ onClick (msg data.name), style "text-align" "left", padding, style "white-space" "nowrap" ]
                [ Emoji.view data
                , div [ style "display" "inline-block", style "vertical-align" "top", style "margin-left" "0.5rem" ]
                    [ Html.text data.name ]
                ]
    in
    div [ class "row" ]
        [ div [ class "dropdown" ]
            [ div [ class "row" ]
                [ button
                    [ class "button"
                    , padding
                    , style "border-top-right-radius" "0"
                    , style "border-bottom-right-radius" "0"
                    ]
                    [ emojis
                        |> List.head
                        |> Maybe.withDefault Emoji.default
                        |> Emoji.view
                    ]
                , input
                    [ onInput msg
                    , onFocus (msg "")
                    , class "input small icon"
                    , style "width" "6rem"
                    , value emoji
                    ]
                    []
                ]
            , div [ class "dropdown-content" ] (List.map emojiItem emojis)
            ]
        ]


durationInput : (String -> Msg) -> Bool -> String -> Html Msg
durationInput msg isSeconds duration =
    input
        [ type_ "number"
        , placeholder
            (if isSeconds then
                "Secs"

             else
                "Mins"
            )
        , onInput msg
        , onFocus (msg "")
        , name "duration"
        , style "width" "3rem"
        , class "input small"
        , value duration
        ]
        []


paceSelect : Maybe Int -> (String -> Msg) -> Activity.Pace -> Html Msg
paceSelect levelM msg pace =
    let
        paceNames =
            Activity.pace.list |> List.map Tuple.first

        paceTimes =
            case levelM of
                Just level ->
                    MPRLevel.trainingPaces ( MPRLevel.Neutral, level )
                        |> Result.map (List.map (\( name, ( minPace, maxPace ) ) -> stripTimeStr maxPace))
                        |> Result.withDefault (List.repeat (List.length Activity.pace.list) "")

                Nothing ->
                    List.repeat (List.length Activity.pace.list) ""
    in
    div [ class "dropdown medium" ]
        [ button [ class "button medium" ]
            [ text (Activity.pace.toString pace) ]
        , div [ class "dropdown-content" ]
            (List.map2
                (\name time ->
                    a [ onClick (msg name), style "text-align" "left" ] [ span [ style "color" "var(--accent-blue)", style "margin-right" "0.5rem" ] [ Html.text time ], Html.text name ]
                )
                paceNames
                paceTimes
            )
        ]


distanceSelect : (String -> Msg) -> Activity.Distance -> Html Msg
distanceSelect msg distance =
    div [ class "dropdown medium" ]
        [ button [ class "button medium" ]
            [ text (Activity.distance.toString distance) ]
        , div [ class "dropdown-content" ]
            (List.map
                (\( distanceOpt, _ ) ->
                    a [ onClick (msg distanceOpt), style "text-align" "left" ] [ Html.text distanceOpt ]
                )
                Activity.distance.list
            )
        ]


viewError : Result FormError Activity -> Html Msg
viewError errorR =
    case errorR of
        Err error ->
            div [ class "error" ] [ text <| errorMessage error ]

        _ ->
            div [ class "error" ] []


errorMessage : FormError -> String
errorMessage error =
    case error of
        EmptyFieldError field ->
            "Please fill in " ++ field ++ " field"

        _ ->
            "There has been an error"
