module ActivityForm exposing (init, initMove, update, view)

import Activity exposing (Activity, ActivityData, ActivityType)
import ActivityForm.Laps as Laps exposing (Laps)
import ActivityShape
import Date exposing (Date)
import Duration
import Effect exposing (Effect)
import Emoji
import Html exposing (Html, a, button, input, text)
import Html.Attributes exposing (class, name, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Json.Decode as Decode
import MPRLevel
import MonoIcons
import Msg exposing (ActivityForm, ActivityState(..), FormError(..), Msg(..))
import Pace
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, iconButton, row, styleIf, viewIf, viewMaybe)
import Store
import Svg exposing (Svg)



{- GOOD EXAMPLE:
   https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Settings.elm
-}


init : Activity -> ActivityForm
init activity =
    let
        laps =
            case activity.laps of
                Just list ->
                    Laps.init list

                Nothing ->
                    Laps.init [ Activity.Individual activity.data ]
    in
    initFromLaps activity laps


initFromLaps : Activity -> Laps -> ActivityForm
initFromLaps activity laps =
    let
        data =
            Laps.get laps
    in
    ActivityForm activity
        (Just activity.date)
        activity.description
        (Ok activity)
        laps
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


update : Msg -> ActivityForm -> ( ActivityForm, Effect )
update msg model =
    case msg of
        ClickedCopy ->
            let
                newLaps =
                    Laps.copy model.laps

                newModel =
                    initFromLaps
                        model.activity
                        newLaps
            in
            ( updateResult newModel
            , Effect.None
            )

        ClickedShift up ->
            let
                newLaps =
                    Laps.shift up model.laps

                newModel =
                    initFromLaps
                        model.activity
                        newLaps
            in
            ( updateResult newModel
            , Effect.None
            )

        ClickedDelete ->
            let
                newLaps =
                    Laps.delete model.laps
            in
            ( updateResult (initFromLaps model.activity newLaps)
            , Effect.None
            )

        SelectedLap index ->
            let
                newLaps =
                    Laps.select index model.laps
            in
            ( updateResult (initFromLaps model.activity newLaps)
            , Effect.None
            )

        ClickedAddLap ->
            let
                newLap =
                    Activity.initActivityData
                        |> (\a -> { a | completed = model.completed })

                newLaps =
                    Laps.add newLap model.laps
            in
            ( updateResult (initFromLaps model.activity newLaps)
            , Effect.None
            )

        SelectedDate date ->
            case model.date of
                Nothing ->
                    let
                        newModel =
                            updateResult { model | date = Just date }
                    in
                    ( newModel, Store.cmd (apply (Move date) newModel) )

                _ ->
                    ( model, Effect.None )

        EditedDescription desc ->
            ( updateResult { model | description = desc }
            , Effect.None
            )

        SelectedEffort effortM ->
            ( updateResult { model | effort = effortM }
            , Effect.None
            )

        SearchedEmojis search ->
            ( updateResult { model | emojiSearch = search }
            , Effect.None
            )

        SelectedEmoji name ->
            ( updateResult { model | emoji = name }
            , Effect.None
            )

        CheckedCompleted ->
            let
                newCompletion =
                    case model.completed of
                        Activity.Completed ->
                            Activity.Planned

                        Activity.Planned ->
                            Activity.Completed

                newLaps =
                    Laps.updateAll (\l -> { l | completed = newCompletion }) model.laps
            in
            ( updateResult (initFromLaps model.activity newLaps)
            , Effect.None
            )

        SelectedActivityType activityType ->
            ( updateResult { model | activityType = activityType }
            , Effect.None
            )

        EditedDuration hms ->
            ( updateResult { model | duration = hms }
            , Effect.None
            )

        SelectedPace str ->
            ( updateResult { model | pace = str }
            , Effect.None
            )

        SelectedRace distM ->
            ( updateResult { model | race = distM }
            , Effect.None
            )

        ClickedMove ->
            ( updateResult { model | date = Nothing }, Effect.None )

        ClickedSubmit ->
            ( model, Store.cmd (apply Update model) )

        _ ->
            ( model, Effect.None )


updateResult : ActivityForm -> ActivityForm
updateResult model =
    let
        laps =
            Laps.set (toActivityData model) model.laps

        ( data, activityLaps ) =
            Laps.toActivityLaps laps

        activity =
            model.activity
                |> (\a ->
                        { a
                            | description = model.description
                            , data = data
                            , laps = activityLaps
                        }
                   )
    in
    { model | laps = laps, activity = activity }
        |> (\m -> { m | result = validate m })


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
    in
    Result.map
        (\date -> { activity | date = date })
        (validateFieldExists model.date "date")


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
            , Html.Attributes.id "activity-form"
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

        Selected _ ->
            row (openAttributes "1.5rem")
                [ column []
                    [ viewMultiSelectButtons ]
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
                        , row []
                            [ viewActivityFields model ]
                        , row [ style "margin-top" "10px", style "margin-bottom" "10px", borderStyle "border-bottom" ]
                            []
                        , expandingRow [ style "overflow" "hidden" ]
                            [ compactColumn [ style "min-width" "4rem", style "overflow-y" "scroll", class "hide-scrollbars", style "padding-left" "3px" ]
                                (List.concat
                                    [ Activity.listLapData model.activity
                                        |> List.indexedMap (\i a -> viewActivityShape levelM (Tuple.first model.laps) i a)
                                    , [ viewAddButton ]
                                    ]
                                )
                            , viewLapFields levelM model
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


viewActivityShape : Maybe Int -> Int -> Int -> ActivityData -> Html Msg
viewActivityShape levelM selectedLap lapIndex activityData =
    row
        [ onClick (SelectedLap lapIndex)
        , attributeIf (selectedLap == lapIndex) (class "selected-shape")
        , style "padding-top" "1rem"
        , style "padding-bottom" "1rem"
        ]
        [ ActivityShape.view levelM activityData ]


viewAddButton : Html Msg
viewAddButton =
    row []
        [ iconButton [ onClick ClickedAddLap ] [ MonoIcons.icon (MonoIcons.add "var(--icon-gray)") ]
        ]


viewActivityFields : ActivityForm -> Html Msg
viewActivityFields form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column []
        [ row [ style "margin-bottom" "10px" ]
            [ column [ maxFieldWidth ] [ dateSelect ClickedMove form.date ]
            , column [ maxFieldWidth ] [ completionToggle CheckedCompleted form.completed ]
            ]
        , row [ style "max-width" "40rem" ]
            [ descriptionInput EditedDescription form.description
            ]
        ]


viewLapFields : Maybe Int -> ActivityForm -> Html Msg
viewLapFields levelM form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column [ style "justify-content" "space-between", style "max-height" "20rem", style "margin-bottom" "10px", style "margin-top" "10px" ]
        [ row []
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
            toolbarButton ClickedSubmit MonoIcons.check "Save" True

          else
            toolbarButton ClickedEdit MonoIcons.edit "Edit" False
        , toolbarButton ClickedCopy MonoIcons.copy "Copy" False
        , toolbarButton ClickedCopy MonoIcons.repeat "Repeat" False
        , toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , column [] []
        , toolbarButton (ClickedShift True) MonoIcons.arrowUp "Shift Up" False
        , toolbarButton (ClickedShift False) MonoIcons.arrowDown "Shift Down" False
        , column [] []
        , toolbarButton ClickedClose MonoIcons.close "Close" False
        ]


toolbarButton : Msg -> (String -> Svg Msg) -> String -> Bool -> Html Msg
toolbarButton onClickMsg icon labelStr primary =
    let
        iconFill =
            if primary then
                "#ffffff"

            else
                "#3d3d3d"
    in
    Html.button
        [ class "button small expand"
        , attributeIf primary (class "primary")
        , Html.Attributes.attribute "aria-label" labelStr
        , style "margin-right" "0.2rem"
        , style "text-align" "center"
        , style "max-width" "3rem"
        , onClick onClickMsg
        ]
        [ MonoIcons.icon (icon iconFill) ]


viewMultiSelectButtons : Html Msg
viewMultiSelectButtons =
    row []
        [ toolbarButton ClickedDelete MonoIcons.delete "Delete" False
        , toolbarButton ClickedGroup MonoIcons.folder "Group" False
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
            , Html.Attributes.id "description"
            , Html.Attributes.attribute "aria-label" "Description"
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
                [ class "button column expand"
                , onClick (msg aType)
                , style "margin-top" "3px"
                , style "margin-right" "3px"
                , style "max-width" "6rem"
                , style "padding-left" "0"
                , style "padding-right" "0"
                , style "align-items" "center"
                , styleIf (activityType == aType) "border" "1px solid var(--accent-blue)"
                ]
                [ MonoIcons.icon (icon aType "#3d3d3d")
                , row
                    [ style "margin-top" "0.1rem"
                    ]
                    [ text (Activity.activityType.toString aType) ]
                ]
    in
    column []
        [ Html.label [] [ text "Type" ]
        , row [ style "flex-wrap" "wrap" ]
            (List.map
                (\( _, aType ) ->
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


completionToggle : Msg -> Activity.Completion -> Html Msg
completionToggle msg completed =
    column []
        [ Html.label [] [ text "Completed" ]
        , Html.input
            [ onClick msg
            , Html.Attributes.attribute "type" "checkbox"
            , style "width" "1.5rem"
            , style "height" "1.5rem"
            , attributeIf (completed == Activity.Completed) (Html.Attributes.attribute "checked" "")
            ]
            []
        ]


emojiSelect : (String -> Msg) -> String -> String -> Html Msg
emojiSelect msg name search =
    let
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

        numberInput nameStr max attrs =
            input
                ([ Html.Attributes.type_ "number"
                 , class "input"
                 , Html.Attributes.min "0"
                 , Html.Attributes.max max
                 , Html.Attributes.step "1"
                 , Html.Attributes.maxlength (String.length max)
                 , Html.Attributes.autocomplete False
                 , name nameStr
                 , Html.Attributes.attribute "aria-label" nameStr
                 , Html.Attributes.id nameStr
                 ]
                    ++ attrs
                )
    in
    column []
        [ label "Time" (hrs /= 0 || mins /= 0 || secs /= 0) (msg ( 0, 0, 0 ))
        , row []
            [ compactColumn [ style "width" "2.5rem" ]
                [ header "HOURS"
                , numberInput "hours"
                    "9"
                    [ onInput (\h -> msg ( String.toInt h |> Maybe.withDefault 0, mins, secs ))
                    , value (toValue hrs)
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "MINS"
                , numberInput "minutes"
                    "60"
                    [ onInput (\m -> msg ( hrs, String.toInt m |> Maybe.withDefault 0, secs ))
                    , value (toValue mins)
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "SECS"
                , numberInput "seconds"
                    "60"
                    [ onInput (\s -> msg ( hrs, mins, String.toInt s |> Maybe.withDefault 0 ))
                    , value (toValue secs)
                    ]
                    []
                ]
            ]
        ]


paceSelect : Maybe Int -> (String -> Msg) -> String -> Html Msg
paceSelect levelM msg paceStr =
    let
        paceTimes =
            case levelM of
                Just level ->
                    Pace.trainingPaces ( MPRLevel.Neutral, level )
                        |> Result.map (List.map (\( name, ( minPace, maxPace ) ) -> Duration.stripTimeStr maxPace))
                        |> Result.withDefault []

                Nothing ->
                    []

        trainingPaceStr =
            parsePace paceStr
                |> Maybe.map2 (\level paceSecs -> Pace.secondsToTrainingPace level paceSecs) levelM
                |> Maybe.map Pace.trainingPace.toString
                |> Maybe.withDefault ""
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
            , row []
                [ input
                    [ onInput msg
                    , class "input"
                    , style "width" "3rem"
                    , value paceStr
                    , Html.Attributes.placeholder "mm:ss"
                    ]
                    []
                , compactColumn [ style "margin-left" "5px", style "font-size" "0.8rem", style "justify-content" "center" ] [ text trainingPaceStr ]
                ]
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
    let
        eventDecoder =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map Activity.raceDistance.fromString
                |> Decode.map msg
    in
    column [ style "max-width" "10rem" ]
        [ label "Race" (distanceM /= Nothing) (msg Nothing)
        , Html.select [ Html.Events.on "change" eventDecoder ]
            (List.map
                (\( distanceStr, distanceOpt ) ->
                    Html.option
                        [ Html.Attributes.value distanceStr
                        , Html.Attributes.selected (distanceOpt == distanceM)
                        ]
                        [ Html.text distanceStr ]
                )
                (( "Select", Nothing ) :: (Activity.raceDistance.list |> List.map (Tuple.mapSecond Just)))
            )
        ]
