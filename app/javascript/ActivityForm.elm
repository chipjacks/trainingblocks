module ActivityForm exposing (init, initMove, update, view)

import Activity
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, ActivityType, LapData(..))
import ActivityForm.Selection as Selection
import ActivityForm.Types exposing (ActivityForm, FieldError(..), Selection, ValidatedFields)
import ActivityForm.Validate as Validate exposing (validate)
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
import Msg exposing (ActivityState(..), Msg(..))
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
            Selection.init (activity.laps |> Maybe.withDefault [ Individual activity.data ])
    in
    initFromSelection activity laps Nothing


initFromSelection : Activity -> Selection LapData -> Maybe (Selection ActivityData) -> ActivityForm
initFromSelection activity laps repeatM =
    let
        lap =
            Selection.get laps
                |> Maybe.withDefault (Individual Activity.initActivityData)

        ( data, newRepeatM, countM ) =
            case ( lap, repeatM ) of
                ( Individual d, _ ) ->
                    ( d, Nothing, Nothing )

                ( Repeats count list, Nothing ) ->
                    ( List.head list |> Maybe.withDefault Activity.initActivityData
                    , Just (Selection.init list)
                    , Just count
                    )

                ( Repeats count list, Just repeat ) ->
                    ( Selection.get repeat |> Maybe.withDefault Activity.initActivityData
                    , Just repeat
                    , Just count
                    )
    in
    ActivityForm activity
        (Just activity.date)
        activity.description
        Validate.init
        laps
        newRepeatM
        (Maybe.map String.fromInt countM)
        data.activityType
        (Maybe.map Duration.toHrsMinsSecs data.duration |> Maybe.map (\( h, m, s ) -> ( String.fromInt h, String.fromInt m, String.fromInt s )) |> Maybe.withDefault ( "", "", "" ))
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


update : Msg -> ActivityForm -> ( ActivityForm, Effect )
update msg model =
    case msg of
        ClickedCopy ->
            ( updateActiveSelection
                (\m ->
                    case m.repeat of
                        Just repeat ->
                            ( m.laps, Just (Selection.copy repeat) )

                        Nothing ->
                            ( Selection.copy m.laps, model.repeat )
                )
                model
                |> updateResult
            , Effect.None
            )

        ClickedRepeat ->
            ( updateActiveSelection
                (\m ->
                    case Selection.get m.laps of
                        Just (Individual data) ->
                            ( Selection.set (Repeats 2 [ data ]) m.laps
                            , Just (Selection.init [ data ])
                            )

                        Just (Repeats count list) ->
                            ( Selection.set (Individual (List.head list |> Maybe.withDefault Activity.initActivityData)) model.laps
                            , Nothing
                            )

                        _ ->
                            ( m.laps, m.repeat )
                )
                model
                |> updateResult
            , Effect.None
            )

        ClickedShift up ->
            ( updateActiveSelection
                (\m ->
                    case m.repeat of
                        Just repeat ->
                            ( m.laps, Just (Selection.shift up repeat) )

                        Nothing ->
                            ( Selection.shift up m.laps, m.repeat )
                )
                model
                |> updateResult
            , Effect.None
            )

        ClickedDelete ->
            ( updateActiveSelection
                (\m ->
                    case m.repeat of
                        Just repeat ->
                            ( m.laps, Just (Selection.delete repeat) )

                        Nothing ->
                            ( Selection.delete m.laps, m.repeat )
                )
                model
                |> updateResult
            , Effect.None
            )

        SelectedLap index ->
            ( updateActiveSelection
                (\m ->
                    let
                        newLaps =
                            Selection.select index m.laps

                        newRepeat =
                            case Selection.get newLaps of
                                Just (Repeats count list) ->
                                    Just (Selection.init list)

                                _ ->
                                    Nothing
                    in
                    ( newLaps, newRepeat )
                )
                model
            , Effect.None
            )

        SelectedRepeatLap index ->
            ( updateRepeat (\repeat -> Selection.select index repeat) model
            , Effect.None
            )

        ClickedAddLap ->
            let
                newData =
                    Activity.initActivityData
                        |> (\a -> { a | completed = model.completed })
            in
            ( updateLaps (\_ laps -> Selection.add (Individual newData) laps) model
                |> updateResult
            , Effect.None
            )

        ClickedAddRepeat ->
            let
                newData =
                    Activity.initActivityData
                        |> (\a -> { a | completed = model.completed, duration = Just 120 })
            in
            ( updateRepeat (\repeat -> Selection.add newData repeat) model
                |> updateResult
            , Effect.None
            )

        SelectedDate date ->
            case model.date of
                Nothing ->
                    let
                        newModel =
                            updateResult { model | date = Just date }
                    in
                    ( newModel, Store.cmd (Move date newModel.activity) )

                _ ->
                    ( model, Effect.None )

        EditedDescription desc ->
            ( updateResult { model | description = desc }
            , Effect.None
            )

        EditedRepeats newCountStr ->
            let
                newModel =
                    { model | repeats = Just newCountStr }
                        |> (\m -> { m | validated = validate m })
            in
            ( updateLaps
                (\lap laps ->
                    case lap of
                        Repeats count list ->
                            Selection.set (Repeats (newModel.validated.repeats |> Result.withDefault count) list) laps

                        _ ->
                            laps
                )
                newModel
                |> updateResult
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
                        Activity.Types.Completed ->
                            Activity.Types.Planned

                        Activity.Types.Planned ->
                            Activity.Types.Completed

                markCompleted data =
                    { data | completed = newCompletion }
            in
            ( updateLaps (\_ laps -> Selection.updateAll (Activity.Laps.updateField markCompleted) laps) model
                |> updateRepeat (\repeat -> Selection.updateAll markCompleted repeat)
                |> updateResult
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
            ( model, Store.cmd (Update model.activity) )

        _ ->
            ( model, Effect.None )


updateActiveSelection : (ActivityForm -> ( Selection LapData, Maybe (Selection ActivityData) )) -> ActivityForm -> ActivityForm
updateActiveSelection transform model =
    let
        ( newLaps, newRepeat ) =
            transform model
    in
    initFromSelection model.activity newLaps newRepeat


updateLaps : (LapData -> Selection LapData -> Selection LapData) -> ActivityForm -> ActivityForm
updateLaps transform model =
    let
        selectedLap =
            Selection.get model.laps
                |> Maybe.withDefault (Individual Activity.initActivityData)

        newLaps =
            transform selectedLap model.laps
    in
    initFromSelection model.activity newLaps model.repeat


updateRepeat : (Selection ActivityData -> Selection ActivityData) -> ActivityForm -> ActivityForm
updateRepeat transform model =
    let
        newRepeat =
            case model.repeat of
                Just repeat ->
                    Just (transform repeat)

                Nothing ->
                    model.repeat
    in
    initFromSelection model.activity model.laps newRepeat


updateResult : ActivityForm -> ActivityForm
updateResult model =
    { model | validated = validate model }
        |> (\m ->
                case ( Selection.get m.laps, m.repeat ) of
                    ( Just (Repeats count list), Just repeat ) ->
                        let
                            selection =
                                Selection.set (toActivityData m) repeat
                        in
                        { m
                            | laps = Selection.set (Repeats count (Selection.toList selection)) m.laps
                            , repeat = Just selection
                        }

                    ( _, _ ) ->
                        { m | laps = Selection.set (Individual <| toActivityData m) m.laps }
           )
        |> (\m ->
                { m
                    | activity = Activity.Laps.set m.activity (Selection.toList m.laps)
                    , description = m.description
                }
           )


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
                            [ column [ style "margin-bottom" "1rem" ]
                                [ viewButtons model.activity True ]
                            ]
                        , row []
                            [ viewActivityFields model ]
                        , row [ style "margin-top" "10px", style "margin-bottom" "5px", borderStyle "border-bottom" ]
                            []
                        , expandingRow [ style "overflow" "hidden" ]
                            [ viewLapShapes levelM model.laps model.repeat
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


viewLapShapes : Maybe Int -> Selection LapData -> Maybe (Selection ActivityData) -> Html Msg
viewLapShapes levelM lapSelection repeatSelectionM =
    compactColumn [ style "min-width" "4rem", style "overflow-y" "scroll", class "hide-scrollbars", style "padding-left" "3px" ]
        (List.concat
            [ Selection.toList lapSelection
                |> List.indexedMap (\i lap -> viewActivityShape levelM (Selection.selectedIndex lapSelection) i lap repeatSelectionM)
                |> List.concat
            , [ viewAddButton ClickedAddLap ]
            ]
        )


viewActivityShape : Maybe Int -> Int -> Int -> LapData -> Maybe (Selection ActivityData) -> List (Html Msg)
viewActivityShape levelM selectedLap lapIndex lap repeatM =
    case ( selectedLap == lapIndex, lap, repeatM ) of
        ( True, Repeats count list, Just repeat ) ->
            List.concat
                [ List.indexedMap
                    (\i data ->
                        row
                            [ onClick (SelectedRepeatLap i)
                            , attributeIf (i == Selection.selectedIndex repeat) (class "selected-shape")
                            , style "padding-top" "0.5rem"
                            , style "padding-bottom" "0.5rem"
                            , style "min-height" "1rem"
                            ]
                            [ ActivityShape.view levelM data ]
                    )
                    (Selection.toList repeat)
                , [ row
                        [ style "padding-top" "0.5rem"
                        , style "padding-bottom" "0.5rem"
                        ]
                        [ viewAddButton ClickedAddRepeat ]
                  ]
                , List.map (\data -> row [ style "opacity" "0.5" ] [ ActivityShape.view levelM data ]) (List.repeat (count - 1) list |> List.concat)
                ]

        _ ->
            case lap of
                Individual data ->
                    List.singleton <|
                        row
                            [ onClick (SelectedLap lapIndex)
                            , attributeIf (selectedLap == lapIndex && repeatM == Nothing) (class "selected-shape")
                            , style "padding-top" "0.5rem"
                            , style "padding-bottom" "0.5rem"
                            , style "min-height" "1rem"
                            ]
                            [ ActivityShape.view levelM data ]

                Repeats count list ->
                    List.map (\data -> row [ onClick (SelectedLap lapIndex) ] [ ActivityShape.view levelM data ]) list
                        |> List.repeat count
                        |> List.concat


viewAddButton : Msg -> Html Msg
viewAddButton msg =
    row []
        [ iconButton [ onClick msg ] [ MonoIcons.icon (MonoIcons.add "var(--icon-gray)") ]
        ]


viewActivityFields : ActivityForm -> Html Msg
viewActivityFields form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column []
        [ row [ style "margin-bottom" "10px", style "max-width" "40rem" ]
            [ descriptionInput EditedDescription form.description
            ]
        , row []
            [ column [ maxFieldWidth ] [ completionToggle CheckedCompleted form.completed ]
            , column [ maxFieldWidth ] [ emojiSelect SelectedEmoji form.emoji form.emojiSearch ]
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
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ activityTypeSelect SelectedActivityType form.activityType ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ repeatsInput EditedRepeats form.repeats form.validated.repeats ]
            ]
        , row []
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ durationInput EditedDuration form.duration ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ effortSelect SelectedEffort form.effort ]
            ]
        , row [ styleIf (form.activityType /= Activity.Types.Run) "visibility" "hidden" ]
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ paceSelect levelM SelectedPace form.pace form.validated.pace ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ raceSelect SelectedRace form.race ]
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


toActivityData : ActivityForm -> ActivityData
toActivityData model =
    Activity.Types.ActivityData
        model.activityType
        (case model.duration of
            ( "", "", "" ) ->
                Nothing

            _ ->
                model.validated.duration |> Result.toMaybe
        )
        model.completed
        (if model.activityType == Activity.Types.Run then
            model.validated.pace |> Result.toMaybe

         else
            Nothing
        )
        (if model.activityType == Activity.Types.Run then
            model.race

         else
            Nothing
        )
        model.effort
        (case model.emoji of
            "" ->
                Nothing

            _ ->
                model.validated.emoji |> Result.toMaybe
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


squareIconButton : Html msg -> String -> List (Html.Attribute msg) -> Html msg
squareIconButton icon name attributes =
    button
        ([ class "button column expand"
         , style "margin-top" "3px"
         , style "margin-right" "3px"
         , style "max-width" "6rem"
         , style "padding-left" "0"
         , style "padding-right" "0"
         , style "align-items" "center"
         ]
            ++ attributes
        )
        [ MonoIcons.icon icon
        , row
            [ style "margin-top" "0.1rem"
            ]
            [ text name ]
        ]


activityTypeSelect : (ActivityType -> Msg) -> ActivityType -> Html Msg
activityTypeSelect msg activityType =
    let
        icon aType =
            case aType of
                Activity.Types.Run ->
                    MonoIcons.stop

                Activity.Types.Other ->
                    MonoIcons.circle

        iconButton aType =
            squareIconButton
                (icon aType "#3d3d3d")
                (Activity.activityType.toString aType)
                [ onClick (msg aType)
                , styleIf (activityType == aType) "border" "1px solid var(--accent-blue)"
                ]
    in
    column []
        [ Html.label [] [ text "Type" ]
        , row []
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


completionToggle : Msg -> Activity.Types.Completion -> Html Msg
completionToggle msg completed =
    column []
        [ Html.label [] [ text "Completed" ]
        , Html.input
            [ onClick msg
            , Html.Attributes.attribute "type" "checkbox"
            , style "width" "1.5rem"
            , style "height" "1.5rem"
            , attributeIf (completed == Activity.Types.Completed) (Html.Attributes.attribute "checked" "")
            ]
            []
        ]


repeatsInput : (String -> Msg) -> Maybe String -> Result FieldError Int -> Html Msg
repeatsInput msg countStrM result =
    column [ style "width" "6rem" ]
        [ label "Repeats" (countStrM /= Just "") ClickedRepeat
        , case countStrM of
            Just countStr ->
                row []
                    [ numberInput "repeats"
                        99
                        [ onInput msg
                        , value countStr
                        ]
                        []
                    ]

            Nothing ->
                row [] [ squareIconButton (MonoIcons.repeat "#3d3d3d") "Repeat" [ onClick ClickedRepeat ] ]
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
                , style "margin-bottom" "5px"
                , style "cursor" "pointer"
                , styleIf (data.name == name) "box-shadow" "0 0 0 0.2rem var(--icon-gray)"
                ]
                [ Emoji.view data ]
    in
    column [ style "width" "150px" ]
        [ row [ style "justify-content" "space-between" ]
            [ label "Feel" (name /= "") (msg "")
            , compactColumn [ borderStyle "border-bottom" ]
                [ row []
                    [ MonoIcons.icon (MonoIcons.search "var(--icon-gray)")
                    , input
                        [ onInput SearchedEmojis
                        , onFocus (SearchedEmojis "")
                        , style "width" "4rem"
                        , style "padding" "0"
                        , style "border" "none"
                        , value search
                        ]
                        []
                    ]
                ]
            ]
        , row [ style "margin-top" "4px", style "height" "27px" ]
            (List.map emojiItem (emojis |> List.take 5))
        ]


durationInput : (( String, String, String ) -> Msg) -> ( String, String, String ) -> Html Msg
durationInput msg ( hrs, mins, secs ) =
    let
        header str =
            row [ style "font-size" "0.6rem", style "color" "var(--icon-gray)", style "margin-bottom" "2px" ]
                [ text str ]
    in
    column []
        [ label "Time" (hrs /= "" || mins /= "" || secs /= "") (msg ( "", "", "" ))
        , row []
            [ compactColumn [ style "width" "2.5rem" ]
                [ header "HOURS"
                , numberInput "hours"
                    9
                    [ onInput (\h -> msg ( h, mins, secs ))
                    , value hrs
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "MINS"
                , numberInput "minutes"
                    60
                    [ onInput (\m -> msg ( hrs, m, secs ))
                    , value mins
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "SECS"
                , numberInput "seconds"
                    60
                    [ onInput (\s -> msg ( hrs, mins, s ))
                    , value secs
                    ]
                    []
                ]
            ]
        ]


numberInput : String -> Int -> List (Html.Attribute Msg) -> (List (Html Msg) -> Html Msg)
numberInput nameStr max attrs =
    input
        ([ Html.Attributes.type_ "number"
         , class "input"
         , Html.Attributes.min "0"
         , Html.Attributes.max (String.fromInt max)
         , Html.Attributes.step "1"
         , Html.Attributes.maxlength (String.length (String.fromInt max))
         , Html.Attributes.autocomplete False
         , name nameStr
         , Html.Attributes.attribute "aria-label" nameStr
         ]
            ++ attrs
        )


paceSelect : Maybe Int -> (String -> Msg) -> String -> Result FieldError Int -> Html Msg
paceSelect levelM msg paceStr result =
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
            Result.toMaybe result
                |> Maybe.map2 (\level paceSecs -> Pace.secondsToTrainingPace level paceSecs) levelM
                |> Maybe.map Pace.trainingPace.toString
                |> Maybe.withDefault ""

        isSlowerThan time =
            case result of
                Err _ ->
                    True

                Ok pace ->
                    (Pace.paceFromString time |> Maybe.withDefault 0) < pace
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
                                    , styleIf (isSlowerThan time)
                                        "background-color"
                                        "var(--icon-gray)"
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


effortSelect : (Maybe Activity.Types.Effort -> Msg) -> Maybe Activity.Types.Effort -> Html Msg
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
                [ ( ActivityShape.colorString ActivityShape.Green, Just Activity.Types.Easy )
                , ( ActivityShape.colorString ActivityShape.Orange, Just Activity.Types.Moderate )
                , ( ActivityShape.colorString ActivityShape.Red, Just Activity.Types.Hard )
                ]
            )
        ]


raceSelect : (Maybe Activity.Types.RaceDistance -> Msg) -> Maybe Activity.Types.RaceDistance -> Html Msg
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
