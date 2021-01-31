module ActivityForm exposing (init, initMove, isEditing, update, view)

import Activity exposing (Activity, ActivityData)
import ActivityShape
import Api
import Array exposing (Array)
import Date exposing (Date)
import Duration
import Emoji
import Html exposing (Html, a, button, div, i, input, span, text)
import Html.Attributes exposing (class, href, id, name, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Http
import Json.Decode as Decode
import MPRLevel
import MonoIcons
import Msg exposing (ActivityForm, ActivityState(..), DataForm(..), FormError(..), Msg(..))
import Pace exposing (Pace)
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, row, viewIf, viewMaybe)
import Store
import Svg exposing (Svg)
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
            Activity.Run seconds paceM completed ->
                RunForm { duration = Duration.toString seconds, pace = Maybe.map Pace.paceToString paceM |> Maybe.withDefault "", completed = completed }

            Activity.Interval seconds paceM completed ->
                IntervalForm { duration = Duration.toString seconds, pace = Maybe.map Pace.paceToString paceM |> Maybe.withDefault "", completed = completed }

            Activity.Race seconds distance_ completed ->
                RaceForm { duration = Duration.toString seconds, distance = distance_, completed = completed }

            Activity.Other seconds completed ->
                OtherForm { duration = Duration.toString seconds, completed = completed }

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

        SelectedShape dataForm ->
            ( updateResult { model | dataForm = dataForm }
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
            RunForm { data | pace = paceStr }

        IntervalForm data ->
            IntervalForm { data | pace = paceStr }

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
        spacing =
            style "margin-left" "0.4rem"

        dataInputs form result =
            case form of
                RunForm { duration, pace } ->
                    [ compactColumn [ spacing ] [ durationInput EditedDuration False duration ]
                    , compactColumn [ spacing ] [ paceSelect levelM SelectedPace pace ]
                    ]

                IntervalForm { duration, pace } ->
                    [ compactColumn [ spacing ] [ durationInput EditedDuration True duration ]
                    , compactColumn [ spacing ] [ paceSelect levelM SelectedPace pace ]
                    ]

                RaceForm { duration, distance } ->
                    [ compactColumn [ spacing ] [ durationInput EditedDuration False duration ]
                    , compactColumn [ spacing ] [ distanceSelect SelectedDistance distance ]
                    , compactColumn [ spacing ]
                        [ viewMaybe (Result.toMaybe result |> Maybe.andThen Activity.mprLevel)
                            (\level -> text <| "Level " ++ String.fromInt level)
                        ]
                    ]

                OtherForm { duration } ->
                    [ compactColumn [ spacing ] [ durationInput EditedDuration False duration ] ]

                NoteForm { emoji } ->
                    [ compactColumn [ spacing ] [ emojiSelect SelectedEmoji emoji ] ]

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
                        [ column []
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
                            , row [ style "flex-wrap" "none", style "align-items" "center" ] <|
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
            toolbarButton ClickedSubmit MonoIcons.check True

          else
            toolbarButton (EditActivity activity) MonoIcons.edit False
        , toolbarButton (ClickedCopy activity) MonoIcons.copy False
        , toolbarButton (Delete activity) MonoIcons.delete False
        , column [] []
        , toolbarButton (ClickedMove activity) MonoIcons.calendar False
        , toolbarButton (Shift True activity) MonoIcons.arrowUp False
        , toolbarButton (Shift False activity) MonoIcons.arrowDown False
        , column [] []
        , case activity.data of
            Activity.Session activities ->
                toolbarButton (ClickedUngroup activity) MonoIcons.folder False

            _ ->
                Html.text ""
        , column [] []
        , toolbarButton ClickedClose MonoIcons.close False
        ]


toolbarButton : Msg -> (String -> Svg Msg) -> Bool -> Html Msg
toolbarButton onClickMsg icon primary =
    let
        iconFill =
            if primary then
                "#ffffff"

            else
                "#3d3d3d"
    in
    a
        [ class "button small expand"
        , attributeIf primary (class "primary")
        , style "margin-right" "0.2rem"
        , style "text-align" "center"
        , style "max-width" "3rem"
        , onClick onClickMsg
        ]
        [ MonoIcons.icon (icon iconFill) ]


viewMultiSelectButtons : List Activity -> Html Msg
viewMultiSelectButtons activities =
    row []
        [ toolbarButton ClickedGroup MonoIcons.folder False
        ]


shapeSelect : ActivityForm -> Html Msg
shapeSelect model =
    let
        { duration, pace, distance, emoji, completed } =
            defaults model.dataForm

        types =
            [ RunForm { duration = duration, pace = pace, completed = completed }
            , IntervalForm { duration = duration, pace = pace, completed = completed }
            , RaceForm { duration = duration, distance = distance, completed = completed }
            , OtherForm { duration = duration, completed = completed }
            , NoteForm { emoji = emoji }
            ]

        typeStr =
            toActivityData model.dataForm |> Activity.activityTypeToString

        buttonContents aType =
            row []
                [ ActivityShape.viewDefault True (toActivityData aType)
                , compactColumn
                    [ style "margin-left" "0.3rem" ]
                    [ text (Activity.activityTypeToString (toActivityData aType)) ]
                ]
    in
    div [ class "dropdown" ]
        [ button [ class "button" ]
            [ buttonContents model.dataForm ]
        , viewIf (typeStr /= "Session")
            (div [ class "dropdown-content" ]
                (List.map
                    (\aType ->
                        a [ onClick (SelectedShape aType) ] [ buttonContents aType ]
                    )
                    types
                )
            )
        ]


type alias Defaults =
    { duration : String, pace : String, distance : Activity.Distance, completed : Bool, emoji : String }


defaults : DataForm -> Defaults
defaults dataForm =
    let
        duration_ =
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

                IntervalForm { pace } ->
                    pace

                _ ->
                    "7:30"

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

                IntervalForm { completed } ->
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
        Duration.fromString str |> Maybe.withDefault 0


parseIntervalDuration : String -> Int
parseIntervalDuration str =
    if String.isEmpty str then
        0

    else
        Pace.paceFromString str |> Maybe.withDefault 0


parsePace : String -> Maybe Int
parsePace str =
    Pace.paceFromString str


toActivityData : DataForm -> ActivityData
toActivityData dataForm =
    case dataForm of
        RunForm { duration, pace, completed } ->
            Activity.Run (parseDuration duration) (parsePace pace) completed

        IntervalForm { duration, pace, completed } ->
            Activity.Interval (parseIntervalDuration duration) (parsePace pace) completed

        RaceForm { duration, distance, completed } ->
            Activity.Race (parseDuration duration) distance completed

        OtherForm { duration, completed } ->
            Activity.Other (parseDuration duration) completed

        NoteForm { emoji } ->
            Activity.Note emoji

        SessionForm data ->
            Activity.Session data


emojiSelect : (String -> Msg) -> String -> Html Msg
emojiSelect msg emoji =
    let
        emojis =
            Emoji.filter (String.toLower emoji) |> List.take 10

        padding =
            style "padding" "8px 0.5rem 2px 0.5rem"

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
                    , class "input icon"
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
        [ onInput msg
        , onFocus (msg "")
        , name "duration"
        , style "width" "4rem"
        , class "input"
        , value duration
        ]
        []


paceSelect : Maybe Int -> (String -> Msg) -> String -> Html Msg
paceSelect levelM msg paceStr =
    let
        trainingPaceStr =
            parsePace paceStr
                |> Maybe.map2 (\level paceSecs -> Pace.secondsToTrainingPace level paceSecs) levelM
                |> Maybe.map Pace.trainingPace.toString
                |> Maybe.withDefault "Pace"

        paceNames =
            List.drop 1 Pace.trainingPace.list
                |> List.map Tuple.first

        paceTimes =
            case levelM of
                Just level ->
                    Pace.trainingPaces ( MPRLevel.Neutral, level )
                        |> Result.map (List.map (\( name, ( minPace, maxPace ) ) -> Duration.stripTimeStr maxPace))
                        |> Result.withDefault []

                Nothing ->
                    []
    in
    div [ class "row" ]
        [ div [ class "dropdown" ]
            [ div [ class "row" ]
                [ button
                    [ class "button"
                    , style "padding-top" "0.6rem"
                    , style "border-top-right-radius" "0"
                    , style "border-bottom-right-radius" "0"
                    ]
                    [ text trainingPaceStr ]
                , input
                    [ onInput msg
                    , onFocus (msg "")
                    , class "input"
                    , style "width" "4rem"
                    , value paceStr
                    ]
                    []
                ]
            , viewMaybe levelM
                (\_ ->
                    div [ class "dropdown-content" ]
                        (List.map2
                            (\time name ->
                                a [ onClick (msg time), style "text-align" "left" ]
                                    [ Html.text name
                                    , span [ style "color" "var(--accent-blue)", style "margin-left" "0.2rem", style "float" "right" ]
                                        [ Html.text time ]
                                    ]
                            )
                            paceTimes
                            paceNames
                        )
                )
            ]
        ]


distanceSelect : (String -> Msg) -> Activity.Distance -> Html Msg
distanceSelect msg distance =
    div [ class "dropdown" ]
        [ button [ class "button" ]
            [ text (Activity.distance.toString distance) ]
        , div [ class "dropdown-content" ]
            (List.map
                (\( distanceOpt, _ ) ->
                    a
                        [ onClick (msg distanceOpt)
                        , style "text-align" "left"
                        ]
                        [ Html.text distanceOpt ]
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
