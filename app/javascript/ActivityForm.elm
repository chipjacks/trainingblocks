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
import Msg exposing (ActivityForm, ActivityState(..), FormError(..), Msg(..))
import Pace exposing (Pace)
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, row, styleIf, viewIf, viewMaybe)
import Store
import Svg exposing (Svg)
import Task exposing (Task)



{- GOOD EXAMPLE:
   https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Settings.elm
-}


init : Activity -> ActivityForm
init activity =
    let
        data =
            activity.data
    in
    ActivityForm activity.id
        (Just activity.date)
        activity.description
        (Ok activity)
        (Duration.toString data.duration)
        data.completed
        (Maybe.map Pace.paceToString data.pace |> Maybe.withDefault "")
        data.distance
        data.effort
        (Maybe.withDefault "" data.emoji)


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
                (toActivityData model)
                Nothing
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

        EditedDescription desc ->
            ( updateResult { model | description = desc }
            , Cmd.none
            )

        SelectedEffort effort ->
            ( updateResult { model | effort = Just effort }
            , Cmd.none
            )

        SelectedEmoji name ->
            ( updateResult { model | emoji = name }
            , Cmd.none
            )

        CheckedCompleted ->
            ( updateResult { model | completed = not model.completed }
            , Cmd.none
            )

        EditedDuration str ->
            ( updateResult { model | duration = str }
            , Cmd.none
            )

        SelectedPace str ->
            ( updateResult { model | pace = str }
            , Cmd.none
            )

        SelectedDistance dist ->
            ( updateResult { model | distance = Just dist }
            , Cmd.none
            )

        ClickedMove ->
            ( updateResult { model | date = Nothing }, Cmd.none )

        ClickedSubmit ->
            ( model, Store.cmd (apply Update model) )

        _ ->
            ( model, Cmd.none )


updateResult : ActivityForm -> ActivityForm
updateResult model =
    { model | result = validate model }


view : Maybe Int -> ActivityState -> Html Msg
view levelM activityM =
    let
        spacing =
            style "margin" "3px 0 0 5px"

        dataInputs form result =
            [ compactColumn [ spacing ] [ dateSelect ClickedMove form.date ]
            , compactColumn [ spacing ] [ completionToggle CheckedCompleted form.completed ]
            , compactColumn [ spacing ] [ durationInput EditedDuration False form.duration ]
            , compactColumn [ spacing ] [ effortSelect SelectedEffort form.effort ]
            , compactColumn [ spacing ] [ paceSelect levelM SelectedPace form.pace ]
            , compactColumn [ spacing ] [ distanceSelect SelectedDistance form.distance ]
            , compactColumn [ spacing ] [ emojiSelect SelectedEmoji form.emoji ]
            ]

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
                            , row [ style "flex-wrap" "wrap", style "margin-left" "-5px" ]
                                (dataInputs model model.result)
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


type alias Defaults =
    { duration : String, pace : String, distance : Activity.Distance, completed : Bool, emoji : String }


defaults : Defaults
defaults =
    Defaults "30" "7:30" Activity.FiveK True Emoji.default.name


parseDuration : String -> Int
parseDuration str =
    if String.isEmpty str then
        0

    else
        Duration.fromString str |> Maybe.withDefault 0


parsePace : String -> Maybe Int
parsePace str =
    Pace.paceFromString str


toActivityData : ActivityForm -> ActivityData
toActivityData model =
    Activity.ActivityData
        (parseDuration model.duration)
        model.completed
        (parsePace model.pace)
        model.distance
        model.effort
        (case model.emoji of
            "" ->
                Nothing

            _ ->
                Just model.emoji
        )


dateSelect : Msg -> Maybe Date -> Html Msg
dateSelect msg date =
    button [ class "button", onClick msg ]
        [ text (Maybe.map (Date.format "E MMM d") date |> Maybe.withDefault "Select Date")
        ]


completionToggle : Msg -> Bool -> Html Msg
completionToggle msg completed =
    button
        [ class "button"
        , onClick CheckedCompleted
        , styleIf (not completed) "background-color" "transparent"
        , styleIf (not completed) "box-shadow" "0 0 0 2px var(--button-gray) inset"
        ]
        [ text
            (if completed then
                "Completed"

             else
                "Planned"
            )
        ]


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
    div [ class "dropdown" ]
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
    div [ class "dropdown" ]
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


effortSelect : (Activity.Effort -> Msg) -> Maybe Activity.Effort -> Html Msg
effortSelect msg effortM =
    div [ class "dropdown" ]
        [ button [ class "button" ]
            [ text (Maybe.map Activity.effort.toString effortM |> Maybe.withDefault "Effort") ]
        , div [ class "dropdown-content" ]
            (List.map
                (\( effortStr, effortOpt ) ->
                    a
                        [ onClick (msg effortOpt)
                        , style "text-align" "left"
                        ]
                        [ Html.text effortStr ]
                )
                Activity.effort.list
            )
        ]


distanceSelect : (Activity.Distance -> Msg) -> Maybe Activity.Distance -> Html Msg
distanceSelect msg distanceM =
    div [ class "dropdown" ]
        [ button [ class "button" ]
            [ text (Maybe.map Activity.distance.toString distanceM |> Maybe.withDefault "Distance") ]
        , div [ class "dropdown-content" ]
            (List.map
                (\( distanceStr, distanceOpt ) ->
                    a
                        [ onClick (msg distanceOpt)
                        , style "text-align" "left"
                        ]
                        [ Html.text distanceStr ]
                )
                Activity.distance.list
            )
        ]
