module ActivityForm exposing (init, initMove, update, view)

import Activity exposing (Activity, ActivityData, ActivityType)
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
        ( lap, data ) =
            case activity.laps of
                Just (first :: rest) ->
                    ( Just 0, first )

                _ ->
                    ( Nothing, activity.data )
    in
    initFromData activity lap data


initFromData : Activity -> Maybe Int -> ActivityData -> ActivityForm
initFromData activity lapM data =
    ActivityForm activity
        (Just activity.date)
        activity.description
        (Ok activity)
        lapM
        data.activityType
        (Maybe.map Duration.toHrsMinsSecs data.duration |> Maybe.withDefault ( 0, 0, 0 ))
        data.completed
        (Maybe.map Pace.paceToString data.pace |> Maybe.withDefault "")
        data.race
        data.effort
        (Maybe.withDefault "" data.emoji)
        ""


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


validateFieldExists : Maybe a -> String -> Result FormError a
validateFieldExists fieldM fieldName =
    case fieldM of
        Just field ->
            Ok field

        Nothing ->
            Err <| EmptyFieldError fieldName


validate : ActivityForm -> Result FormError Activity
validate model =
    let
        activity =
            model.activity

        lapsM =
            Maybe.map2
                (\index laps ->
                    Array.fromList laps
                        |> Array.set index (toActivityData model)
                        |> Array.toList
                )
                model.lap
                activity.laps

        data =
            case lapsM of
                Just laps ->
                    sumLapData laps

                Nothing ->
                    toActivityData model
    in
    -- TODO : Validate run has two of three fields (pace, distance, time)
    Result.map
        (\date ->
            { activity
                | date = date
                , description = model.description
                , data = data
                , laps = lapsM
            }
        )
        (validateFieldExists model.date "date")


sumLapData : List ActivityData -> ActivityData
sumLapData laps =
    let
        duration =
            List.filterMap .duration laps |> List.sum

        completed =
            List.all .completed laps
    in
    ActivityData
        Activity.Run
        (Just duration)
        completed
        Nothing
        Nothing
        Nothing
        Nothing


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

        SelectedLap index ->
            let
                lapDataM =
                    model.activity.laps
                        |> Maybe.map (\laps -> Array.fromList laps |> Array.get index)

                newModel =
                    case lapDataM of
                        Just (Just lap) ->
                            initFromData model.activity (Just index) lap

                        _ ->
                            model
            in
            ( updateResult newModel
            , Cmd.none
            )

        SelectedEffort effortM ->
            ( updateResult { model | effort = effortM }
            , Cmd.none
            )

        SearchedEmojis search ->
            ( updateResult { model | emojiSearch = search }
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

        SelectedActivityType activityType ->
            ( updateResult { model | activityType = activityType }
            , Cmd.none
            )

        EditedDuration hms ->
            ( updateResult { model | duration = hms }
            , Cmd.none
            )

        SelectedPace str ->
            ( updateResult { model | pace = str }
            , Cmd.none
            )

        SelectedRace distM ->
            ( updateResult { model | race = distM }
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
    let
        result =
            validate model

        activity =
            Result.withDefault model.activity result
    in
    { model | result = validate model, activity = activity }


view : Maybe Int -> ActivityState -> Html Msg
view levelM activityM =
    let
        sharedAttributes =
            [ borderStyle "border-bottom"
            , style "position" "absolute"
            , style "left" "0"
            , style "right" "0"
            , style "background-color" "white"
            , style "z-index" "2"
            , style "overflow" "hidden"
            , style "transition" "height 0.5s"
            ]

        openAttributes height =
            [ style "height" height
            , style "padding" "0.5rem 0.5rem"
            , style "border-width" "1px"
            ]
                ++ sharedAttributes

        closedAttributes =
            [ style "border-width" "0"
            , style "height" "0"
            ]
                ++ sharedAttributes
    in
    case activityM of
        Selected [ activity ] ->
            row (openAttributes "1.5rem")
                [ column []
                    [ viewButtons activity False ]
                ]

        Selected activities ->
            row (openAttributes "1.5rem")
                [ column []
                    [ viewMultiSelectButtons activities ]
                ]

        Editing model ->
            if model.date /= Nothing then
                row (openAttributes "100%")
                    [ column []
                        [ row []
                            [ viewMaybe (Result.toMaybe model.result)
                                (\activity ->
                                    column [ style "margin-bottom" "1rem" ]
                                        [ viewButtons activity True ]
                                )
                            ]
                        , expandingRow [ style "overflow" "hidden" ]
                            [ compactColumn [ style "min-width" "4rem", style "overflow-y" "scroll", class "hide-scrollbars", style "padding-left" "3px" ]
                                (Maybe.withDefault [ model.activity.data ] model.activity.laps
                                    |> List.indexedMap (\i a -> viewActivityShape levelM model.lap i a)
                                )
                            , viewFormFields levelM model
                            ]
                        ]
                    ]

            else
                row (openAttributes "1.5rem")
                    [ row []
                        [ MonoIcons.icon (MonoIcons.circleInformation "var(--accent-blue)")
                        , column [ style "margin-left" "1rem" ] [ text "Select Date" ]
                        ]
                    ]

        _ ->
            row closedAttributes []


viewActivityShape : Maybe Int -> Maybe Int -> Int -> ActivityData -> Html Msg
viewActivityShape levelM selectedLapM lapIndex activityData =
    row
        [ onClick (SelectedLap lapIndex)
        , attributeIf (selectedLapM == Just lapIndex) (class "selected-shape")
        , style "padding-top" "1rem"
        , style "padding-bottom" "1rem"
        ]
        [ ActivityShape.view levelM activityData ]


viewFormFields : Maybe Int -> ActivityForm -> Html Msg
viewFormFields levelM form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column [ style "justify-content" "space-between", style "max-height" "35rem" ]
        [ row []
            [ column [ maxFieldWidth ] [ dateSelect ClickedMove form.date ]
            , column [ maxFieldWidth ] [ completionToggle CheckedCompleted form.completed ]
            ]
        , row [ style "max-width" "40rem" ]
            [ descriptionInput EditedDescription form.description
            ]
        , row []
            [ column [ maxFieldWidth ] [ activityTypeSelect SelectedActivityType form.activityType ]
            , column [ maxFieldWidth ] [ durationInput EditedDuration form.duration ]
            ]
        , row []
            [ column [ maxFieldWidth ] [ effortSelect SelectedEffort form.effort ]
            , column [ maxFieldWidth ] [ emojiSelect SelectedEmoji form.emoji form.emojiSearch ]
            ]
        , row [ styleIf (form.activityType /= Activity.Run) "visibility" "hidden" ]
            [ column [ maxFieldWidth ] [ paceSelect levelM SelectedPace form.pace ]
            , column [ maxFieldWidth ] [ raceSelect SelectedRace form.race ]
            ]
        ]


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


parseDuration : ( Int, Int, Int ) -> Maybe Int
parseDuration ( hrs, mins, secs ) =
    let
        seconds =
            hrs * 60 * 60 + mins * 60 + secs
    in
    if seconds == 0 then
        Nothing

    else
        Just seconds


parsePace : String -> Maybe Int
parsePace str =
    Pace.paceFromString str


toActivityData : ActivityForm -> ActivityData
toActivityData model =
    Activity.ActivityData
        model.activityType
        (parseDuration model.duration)
        model.completed
        (if model.activityType == Activity.Run then
            parsePace model.pace

         else
            Nothing
        )
        (if model.activityType == Activity.Run then
            model.race

         else
            Nothing
        )
        model.effort
        (case model.emoji of
            "" ->
                Nothing

            _ ->
                Just model.emoji
        )


label : String -> Bool -> Msg -> Html Msg
label name showClear onClear =
    row []
        [ Html.label [] [ text name ]
        , viewIf showClear
            (compactColumn
                [ style "margin-left" "0.2rem", style "cursor" "pointer", onClick onClear ]
                [ MonoIcons.icon (MonoIcons.close "var(--icon-gray)")
                ]
            )
        ]


descriptionInput : (String -> Msg) -> String -> Html Msg
descriptionInput msg str =
    column []
        [ label "Description" (str /= "") (msg "")
        , input
            [ type_ "text"
            , Html.Attributes.autocomplete False
            , onInput msg
            , name "description"
            , value str
            ]
            []
        ]


activityTypeSelect : (ActivityType -> Msg) -> ActivityType -> Html Msg
activityTypeSelect msg activityType =
    let
        icon aType =
            case aType of
                Activity.Run ->
                    MonoIcons.stop

                Activity.Other ->
                    MonoIcons.circle

        iconButton aType =
            button
                [ class "button"
                , onClick (SelectedActivityType aType)
                , style "margin-top" "3px"
                , style "margin-right" "3px"
                , style "width" "6rem"
                , style "justify-content" "center"
                , styleIf (activityType == aType) "border" "1px solid var(--accent-blue)"
                ]
                [ MonoIcons.icon (icon aType "#3d3d3d")
                , compactColumn
                    [ style "margin-top" "0.1rem"
                    ]
                    [ text (Activity.activityType.toString aType) ]
                ]
    in
    column []
        [ Html.label [] [ text "Type" ]
        , row [ style "flex-wrap" "wrap" ]
            (List.map
                (\( str, aType ) ->
                    iconButton aType
                )
                Activity.activityType.list
            )
        ]


dateSelect : Msg -> Maybe Date -> Html Msg
dateSelect msg date =
    column []
        [ Html.label [] [ text "Date" ]
        , row []
            [ button [ class "button", onClick msg ]
                [ text (Maybe.map (Date.format "E MMM d") date |> Maybe.withDefault "Select Date")
                ]
            ]
        ]


completionToggle : Msg -> Bool -> Html Msg
completionToggle msg completed =
    column []
        [ Html.label [] [ text "Completed" ]
        , Html.input
            [ onClick CheckedCompleted
            , Html.Attributes.attribute "type" "checkbox"
            , style "width" "1.5rem"
            , style "height" "1.5rem"
            , attributeIf completed (Html.Attributes.attribute "checked" "")
            ]
            []
        ]


emojiSelect : (String -> Msg) -> String -> String -> Html Msg
emojiSelect msg name search =
    let
        selected =
            Emoji.find name

        emojis =
            if String.isEmpty search then
                Emoji.recommended

            else
                Emoji.filter (String.toLower search)

        emojiItem data =
            compactColumn
                [ onClick (msg data.name)
                , style "border-radius" "11px"
                , style "width" "22px"
                , style "height" "22px"
                , style "margin-right" "10px"
                , style "margin-top" "5px"
                , style "margin-bottom" "5px"
                , style "cursor" "pointer"
                , styleIf (data.name == name) "box-shadow" "0 0 0 0.2rem var(--icon-gray)"
                ]
                [ Emoji.view data ]
    in
    column []
        [ label "Emoji" (name /= "") (msg "")
        , row [ style "margin-top" "4px", style "flex-wrap" "wrap" ]
            (List.map emojiItem (emojis |> List.take 5))
        , row []
            [ input
                [ onInput SearchedEmojis
                , onFocus (SearchedEmojis "")
                , Html.Attributes.placeholder "Search"
                , style "width" "6rem"
                , value search
                ]
                []
            ]
        ]


durationInput : (( Int, Int, Int ) -> Msg) -> ( Int, Int, Int ) -> Html Msg
durationInput msg ( hrs, mins, secs ) =
    let
        toValue int =
            if int == 0 then
                ""

            else
                String.fromInt int

        header str =
            row [ style "font-size" "0.6rem", style "color" "var(--icon-gray)", style "margin-bottom" "2px" ]
                [ text str ]

        numberInput max attrs =
            input
                ([ Html.Attributes.type_ "number"
                 , class "input"
                 , Html.Attributes.min "0"
                 , Html.Attributes.max max
                 , Html.Attributes.step "1"
                 , Html.Attributes.maxlength (String.length max)
                 , Html.Attributes.autocomplete False
                 ]
                    ++ attrs
                )
    in
    column []
        [ label "Time" (hrs /= 0 || mins /= 0 || secs /= 0) (msg ( 0, 0, 0 ))
        , row []
            [ compactColumn [ style "width" "2.5rem" ]
                [ header "HOURS"
                , numberInput "9"
                    [ onInput (\h -> msg ( String.toInt h |> Maybe.withDefault 0, mins, secs ))
                    , value (toValue hrs)
                    , name "hours"
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "MINS"
                , numberInput "60"
                    [ onInput (\m -> msg ( hrs, String.toInt m |> Maybe.withDefault 0, secs ))
                    , value (toValue mins)
                    , name "minutes"
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "SECS"
                , numberInput "60"
                    [ onInput (\s -> msg ( hrs, mins, String.toInt s |> Maybe.withDefault 0 ))
                    , value (toValue secs)
                    , name "seconds"
                    ]
                    []
                ]
            ]
        ]


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
    column []
        [ label "Pace" (paceStr /= "") (msg "")
        , column []
            [ viewMaybe levelM
                (\_ ->
                    row [ style "margin-top" "2px", style "margin-bottom" "2px", style "border-radius" "4px", style "overflow" "hidden", style "max-width" "10rem", style "margin-right" "10px" ]
                        (List.map
                            (\time ->
                                column
                                    [ style "background-color" (ActivityShape.colorString ActivityShape.Green)
                                    , onClick (msg time)
                                    , style "height" "0.5rem"
                                    , style "margin-right" "1px"
                                    , style "cursor" "pointer"
                                    , styleIf ((parsePace time |> Maybe.withDefault 0) < (parsePace paceStr |> Maybe.withDefault 0) || (parsePace paceStr == Nothing)) "background-color" "var(--icon-gray)"
                                    ]
                                    []
                            )
                            paceTimes
                        )
                )
            , input
                [ onInput msg
                , onFocus (msg "")
                , class "input"
                , style "width" "4rem"
                , value paceStr
                , Html.Attributes.placeholder "mm:ss"
                ]
                []
            ]
        ]


effortSelect : (Maybe Activity.Effort -> Msg) -> Maybe Activity.Effort -> Html Msg
effortSelect msg effortM =
    column []
        [ label "Effort" (effortM /= Nothing) (msg Nothing)
        , row [ style "margin-top" "0.4rem" ]
            (List.map
                (\( color, effortOpt ) ->
                    compactColumn
                        [ style "background-color" color
                        , onClick (msg effortOpt)
                        , style "border-radius" "9px"
                        , style "width" "18px"
                        , style "height" "18px"
                        , style "margin-right" "8px"
                        , style "cursor" "pointer"
                        , styleIf (effortOpt == effortM) "box-shadow" ("0 0 0 2px #FFFFFF, 0 0 0 4px " ++ color)
                        ]
                        []
                )
                [ ( ActivityShape.colorString ActivityShape.Green, Just Activity.Easy )
                , ( ActivityShape.colorString ActivityShape.Orange, Just Activity.Moderate )
                , ( ActivityShape.colorString ActivityShape.Red, Just Activity.Hard )
                ]
            )
        ]


raceSelect : (Maybe Activity.RaceDistance -> Msg) -> Maybe Activity.RaceDistance -> Html Msg
raceSelect msg distanceM =
    column []
        [ label "Race" (distanceM /= Nothing) (msg Nothing)
        , div [ class "dropdown" ]
            [ button [ class "button" ]
                [ text (Maybe.map Activity.raceDistance.toString distanceM |> Maybe.withDefault "Select") ]
            , div [ class "dropdown-content" ]
                (List.map
                    (\( distanceStr, distanceOpt ) ->
                        a
                            [ onClick (msg (Just distanceOpt))
                            , style "text-align" "left"
                            ]
                            [ Html.text distanceStr ]
                    )
                    Activity.raceDistance.list
                )
            ]
        ]
