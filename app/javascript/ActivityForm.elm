module ActivityForm exposing (init, initMove, update, view)

import Actions exposing (actionButton)
import Activity
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, ActivityType, Completion(..), DistanceUnits(..), LapData(..))
import Activity.View
import ActivityForm.Selection as Selection
import ActivityForm.Types exposing (ActivityForm, FieldError(..), Selection, ValidatedFields)
import ActivityForm.Validate as Validate exposing (validate)
import ActivityShape
import Date exposing (Date)
import Distance
import Duration
import Effect exposing (Effect)
import Emoji exposing (EmojiDict)
import EmojiData exposing (EmojiData)
import Html exposing (Html, a, button, input, text)
import Html.Attributes exposing (class, name, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onFocus, onInput)
import Json.Decode as Decode
import MPRLevel
import MonoIcons
import Msg exposing (ActivityConfigs, ActivityState(..), Msg(..))
import Pace
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, iconButton, row, stopPropagationOnClick, styleIf, viewIf, viewMaybe)
import Store
import Svg exposing (Svg)


init : Activity -> ActivityForm
init activity =
    let
        ( editing, completion, laps ) =
            case ( activity.laps, activity.planned ) of
                ( [ lap ], [] ) ->
                    ( False, Completed, Selection.init [ lap ] )

                ( lap :: more, _ ) ->
                    ( False, Completed, Selection.init (lap :: more) )

                ( [], [ lap ] ) ->
                    ( False, Planned, Selection.init [ lap ] )

                ( _, lap :: more ) ->
                    ( False, Planned, Selection.init (lap :: more) )

                ( _, _ ) ->
                    ( False, Completed, Selection.init activity.laps )
    in
    initFromSelection activity editing completion laps Nothing


initFromSelection : Activity -> Bool -> Completion -> Selection LapData -> Maybe (Selection ActivityData) -> ActivityForm
initFromSelection activity editingLap completion laps repeatM =
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
                    , if editingLap then
                        Just (Selection.init list)

                      else
                        Nothing
                    , Just count
                    )

                ( Repeats count list, Just repeat ) ->
                    ( Selection.get repeat |> Maybe.withDefault Activity.initActivityData
                    , if editingLap then
                        Just repeat

                      else
                        Nothing
                    , Just count
                    )

        stringFromInt int =
            if int == 0 then
                ""

            else
                String.fromInt int

        duration =
            Maybe.map Duration.toHrsMinsSecs data.duration
                |> Maybe.map (\( h, m, s ) -> ( stringFromInt h, stringFromInt m, stringFromInt s ))
                |> Maybe.withDefault ( "", "", "" )

        distanceUnits =
            data.distanceUnits |> Maybe.withDefault Activity.Types.Miles

        distance =
            Maybe.map (Distance.fromMeters distanceUnits) data.distance
                |> Maybe.map Distance.round1
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault ""
    in
    { activity = activity
    , editingLap = editingLap
    , laps = laps
    , repeat = newRepeatM
    , validated = Validate.init
    , date = Just activity.date
    , description = activity.description
    , repeats = Maybe.map String.fromInt countM
    , activityType = data.activityType
    , duration = duration
    , completed = completion
    , pace =
        Maybe.map Pace.paceToString data.pace |> Maybe.withDefault ""
    , distance = distance
    , distanceUnits = distanceUnits
    , race = data.race
    , effort = data.effort
    , emoji = Maybe.withDefault "" data.emoji
    , emojiSearch = ""
    }
        |> updateValidated


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
        ClickedEdit ->
            ( { model | editingLap = not model.editingLap }
                |> updateFromSelection
            , Effect.None
            )

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
                |> updateFromSelection
                |> updateActivity
            , Effect.None
            )

        ClickedRepeat ->
            ( updateActiveSelection
                (\m ->
                    case Selection.get m.laps of
                        Just (Individual data) ->
                            ( Selection.set (Repeats 4 [ data ]) m.laps
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
                |> updateRepeat
                    (\repeat ->
                        let
                            restInterval =
                                Activity.initActivityData
                                    |> (\a -> { a | completed = model.completed, duration = Just 120 })
                        in
                        Selection.add restInterval repeat
                            |> Selection.select 0
                    )
                |> updateFromSelection
                |> updateActivity
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
                |> updateActivity
            , Effect.None
            )

        ClickedDelete ->
            let
                closeFormUnlessRepeat m =
                    case m.repeat of
                        Nothing ->
                            { m | editingLap = False }

                        _ ->
                            { m | editingLap = m.editingLap }
            in
            ( updateActiveSelection
                (\m ->
                    case m.repeat of
                        Just repeat ->
                            if List.length (Selection.toList repeat) == 1 then
                                ( Selection.delete m.laps, Nothing )

                            else
                                ( m.laps, Just (Selection.delete repeat) )

                        Nothing ->
                            ( Selection.delete m.laps, m.repeat )
                )
                model
                |> closeFormUnlessRepeat
                |> updateFromSelection
                |> updateActivity
            , Effect.None
            )

        SelectedLap index ->
            ( updateActiveSelection
                (\m ->
                    let
                        newLaps =
                            Selection.select index m.laps
                    in
                    ( newLaps, Nothing )
                )
                model
                |> updateFromSelection
            , Effect.None
            )

        SelectedRepeatLap index ->
            ( updateRepeat (\repeat -> Selection.select index repeat) model
                |> updateFromSelection
            , Effect.None
            )

        ClickedAddLap ->
            let
                newData =
                    Activity.initActivityData
                        |> (\a -> { a | completed = model.completed })
            in
            ( updateActiveSelection
                (\m -> ( Selection.add (Individual newData) m.laps, m.repeat ))
                { model | editingLap = True }
                |> updateFromSelection
                |> updateActivity
            , Effect.None
            )

        ClickedAddRepeat ->
            let
                newData =
                    Activity.initActivityData
                        |> (\a -> { a | completed = model.completed, duration = Just 120 })
            in
            ( updateRepeat (\repeat -> Selection.add newData repeat) model
                |> updateFromSelection
                |> updateActivity
            , Effect.None
            )

        SelectedDate date ->
            case model.date of
                Nothing ->
                    let
                        newModel =
                            updateActivity { model | date = Just date }
                    in
                    ( newModel, Store.cmd (Move date newModel.activity) )

                _ ->
                    ( model, Effect.None )

        EditedDescription desc ->
            ( updateActivity { model | description = desc }
            , Effect.None
            )

        EditedRepeats newCountStr ->
            let
                newModel =
                    { model | repeats = Just newCountStr }
                        |> updateValidated
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
                |> updateActivity
            , Effect.None
            )

        SelectedEffort effortM ->
            ( updateActivity { model | effort = effortM }
            , Effect.None
            )

        SearchedEmojis search ->
            ( updateActivity { model | emojiSearch = search }
            , Effect.None
            )

        SelectedEmoji name ->
            ( updateActivity { model | emoji = name }
            , Effect.None
            )

        CheckedCompleted ->
            let
                ( newCompletion, newLaps ) =
                    case model.completed of
                        Activity.Types.Completed ->
                            ( Activity.Types.Planned, model.activity.planned )

                        Activity.Types.Planned ->
                            ( Activity.Types.Completed, model.activity.laps )

                newEditingLap =
                    if List.isEmpty newLaps then
                        False

                    else
                        model.editingLap
            in
            ( updateActiveSelection
                (\m ->
                    ( Selection.init newLaps
                    , Nothing
                    )
                )
                { model | completed = newCompletion, editingLap = newEditingLap }
                |> updateFromSelection
            , Effect.None
            )

        ClickedAutofill ->
            let
                updateCompleted data =
                    { data | completed = model.completed }

                newLaps =
                    case model.completed of
                        Activity.Types.Completed ->
                            model.activity.planned
                                |> List.map (Activity.Laps.updateField updateCompleted)

                        Activity.Types.Planned ->
                            model.activity.laps
                                |> List.map (Activity.Laps.updateField updateCompleted)
            in
            ( updateActiveSelection (\m -> ( Selection.init newLaps, Nothing )) model
                |> updateFromSelection
                |> updateActivity
            , Effect.None
            )

        ClickedClearLaps ->
            ( updateActiveSelection (\m -> ( Selection.init [], Nothing )) { model | editingLap = False }
                |> updateActivity
            , Effect.None
            )

        SelectedActivityType activityType ->
            ( updateActivity { model | activityType = activityType }
            , Effect.None
            )

        EditedDuration hms ->
            ( updateActivity { model | duration = hms }
            , Effect.None
            )

        SelectedPace str ->
            ( updateActivity { model | pace = str }
            , Effect.None
            )

        EditedDistance dist ->
            ( updateActivity { model | distance = dist }
            , Effect.None
            )

        SelectedDistanceUnits units ->
            ( updateActivity { model | distanceUnits = units }
            , Effect.None
            )

        CheckedRace ->
            let
                toggledValue =
                    case model.race of
                        Nothing ->
                            Just Activity.Types.OtherDistance

                        Just _ ->
                            Nothing
            in
            ( updateActivity { model | race = toggledValue }
            , Effect.None
            )

        ClickedMove ->
            ( updateActivity { model | date = Nothing }, Effect.None )

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
    { model | laps = newLaps, repeat = newRepeat }


updateLaps : (LapData -> Selection LapData -> Selection LapData) -> ActivityForm -> ActivityForm
updateLaps transform model =
    let
        newLaps =
            case Selection.get model.laps of
                Just lap ->
                    transform lap model.laps

                Nothing ->
                    model.laps
    in
    { model | laps = newLaps }


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
    { model | repeat = newRepeat }


updateFromSelection : ActivityForm -> ActivityForm
updateFromSelection model =
    initFromSelection model.activity model.editingLap model.completed model.laps model.repeat


updateValidated : ActivityForm -> ActivityForm
updateValidated model =
    { model | validated = validate model }


updateRace : ActivityForm -> ActivityForm
updateRace model =
    let
        newRace =
            case ( model.validated.distance, model.race ) of
                ( Ok distance, Just race ) ->
                    Distance.toRaceDistance (Distance.toMeters model.distanceUnits distance)
                        |> Just

                _ ->
                    model.race
    in
    { model | race = newRace }


updateActivity : ActivityForm -> ActivityForm
updateActivity model =
    let
        updateActivityLaps laps activity =
            case model.completed of
                Activity.Types.Planned ->
                    { activity | planned = Selection.toList laps }

                Activity.Types.Completed ->
                    { activity | laps = Selection.toList laps }

        updateActivityDescription description activity =
            { activity | description = description }

        updateActivityDate dateM activity =
            case dateM of
                Just date ->
                    { activity | date = date }

                _ ->
                    activity
    in
    updateValidated model
        |> updateRace
        |> updateActiveSelection
            (\m ->
                case ( Selection.get m.laps, m.repeat ) of
                    ( Just (Repeats count list), Just repeat ) ->
                        let
                            selection =
                                Selection.set (toActivityData m) repeat
                        in
                        ( Selection.set (Repeats count (Selection.toList selection)) m.laps
                        , Just selection
                        )

                    ( Just (Individual _), _ ) ->
                        ( Selection.set (Individual <| toActivityData m) m.laps, m.repeat )

                    ( _, _ ) ->
                        ( m.laps, m.repeat )
            )
        |> (\m ->
                { m
                    | activity =
                        updateActivityLaps m.laps m.activity
                            |> updateActivityDescription m.description
                            |> updateActivityDate m.date
                }
           )


view : ActivityConfigs -> ActivityState -> Html Msg
view configs activityM =
    let
        padding =
            style "padding" "0.5rem 0.5rem"

        isAutofillable model =
            if List.isEmpty model.activity.laps && List.isEmpty model.activity.planned then
                False

            else
                True
    in
    case activityM of
        Editing model ->
            if model.date == Nothing then
                row [ class "toast", padding, borderStyle "border" ]
                    [ MonoIcons.icon (MonoIcons.circleInformation "var(--blue-500)")
                    , column [ style "margin-left" "1rem" ] [ text "Select Date" ]
                    ]

            else
                row [ class "dimmer", stopPropagationOnClick (Decode.succeed ClickedClose) ]
                    [ column [ class "modal", Html.Attributes.id "activity-form", stopPropagationOnClick (Decode.succeed NoOp) ]
                        [ row [ padding ]
                            [ viewActivityFields configs.emojis model ]
                        , expandingRow [ style "overflow" "hidden", borderStyle "border-top", style "position" "relative" ]
                            [ viewLaps configs
                                model.completed
                                model.editingLap
                                (isAutofillable model)
                                model.laps
                                model.repeat
                            , compactColumn
                                [ style "position" "absolute"
                                , style "top" "0"
                                , style "z-index" "5"
                                , style "background-color" "white"
                                , style "height" "100%"
                                , style "transition" "right 0.5s"
                                , if model.editingLap then
                                    style "right" "0"

                                  else
                                    style "right" "-100%"
                                , style "width" "min(100% - 5rem, 25rem)"
                                , padding
                                , borderStyle "border-left"
                                ]
                                [ viewLapFields configs model
                                ]
                            ]
                        ]
                    ]

        _ ->
            text ""


viewLaps : ActivityConfigs -> Completion -> Bool -> Bool -> Selection LapData -> Maybe (Selection ActivityData) -> Html Msg
viewLaps configs completed editingLap isAutofillable lapSelection repeatSelectionM =
    let
        repeatM =
            if not editingLap then
                Nothing

            else
                repeatSelectionM

        viewLap index lap =
            Activity.View.listItem
                { titleM = Nothing
                , subtitle = Activity.View.lapDescription configs.levelM lap
                , isActive = Selection.selectedIndex lapSelection == index
                , handlePointerDown = Decode.succeed (SelectedLap index)
                , handleDoubleClick = ClickedEdit
                , handleMultiSelectM = Nothing
                , viewToolbarM =
                    if Selection.selectedIndex lapSelection == index && not editingLap then
                        Just (Actions.viewLapActions False)

                    else
                        Nothing
                , viewShape =
                    compactColumn [] (viewActivityShape configs (Selection.selectedIndex lapSelection) index lap repeatM)
                }
    in
    column
        [ style "overflow-y" "scroll"
        , style "overflow-x" "hidden"
        ]
        (List.concat
            [ [ row
                    [ style "position" "sticky"
                    , style "top" "0"
                    , style "z-index" "4"
                    , style "padding" "0.5rem"
                    , style "height" "1.8rem"
                    , style "align-items" "space-between"
                    , style "background-color" "white"
                    ]
                    [ completionToggle CheckedCompleted completed
                    , viewIf ((Selection.toList lapSelection |> List.isEmpty) && isAutofillable)
                        (Html.button [ class "button medium", onClick ClickedAutofill ] [ text "Autofill" ])
                    , viewIf (not (Selection.toList lapSelection |> List.isEmpty))
                        (Html.button [ class "button medium", onClick ClickedClearLaps ] [ text "Clear" ])
                    ]
              ]
            , Selection.toList lapSelection
                |> List.indexedMap viewLap
            , [ row [ style "padding" "0.5rem 0.5rem" ] [ viewAddButton ClickedAddLap ] ]
            ]
        )


viewActivityShape : ActivityConfigs -> Int -> Int -> LapData -> Maybe (Selection ActivityData) -> List (Html Msg)
viewActivityShape configs selectedLap lapIndex lap repeatM =
    case ( selectedLap == lapIndex, lap, repeatM ) of
        ( True, Repeats count list, Just repeat ) ->
            List.concat
                [ List.indexedMap
                    (\i data ->
                        row
                            [ onClick (SelectedRepeatLap i)
                            , attributeIf (i == Selection.selectedIndex repeat) (class "selected-shape")
                            , attributeIf (i == Selection.selectedIndex repeat) (class "dynamic-shape")
                            , style "padding-top" "0.5rem"
                            , style "padding-bottom" "0.5rem"
                            , style "min-height" "1rem"
                            ]
                            [ ActivityShape.view configs data ]
                    )
                    (Selection.toList repeat)
                , [ row [] [ viewAddButton ClickedAddRepeat ]
                  ]
                ]

        _ ->
            case lap of
                Individual data ->
                    List.singleton <|
                        row
                            [ onClick (SelectedLap lapIndex)
                            , attributeIf (lapIndex == selectedLap) (class "dynamic-shape")
                            , style "padding-top" "0.5rem"
                            , style "padding-bottom" "0.5rem"
                            , style "min-height" "1rem"
                            ]
                            [ ActivityShape.view configs data ]

                Repeats count list ->
                    [ row [ style "padding-top" "0.5rem" ] [] ]
                        ++ List.map (\data -> row [ onClick (SelectedLap lapIndex) ] [ ActivityShape.view configs data ]) list
                        ++ [ row [ style "padding" "5px", style "color" "var(--black-300)", style "font-size" "0.8rem" ] [ text ("x " ++ String.fromInt count) ] ]


viewAddButton : Msg -> Html Msg
viewAddButton msg =
    Actions.viewAddAction msg "Add Lap"


viewActivityFields : EmojiDict -> ActivityForm -> Html Msg
viewActivityFields emojis form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column []
        [ row [ style "margin-bottom" "10px" ]
            [ column [ maxFieldWidth ] [ dateSelect ClickedMove form.date ]
            , column [ style "align-items" "flex-end" ] [ Actions.viewFormActions ]
            ]
        , row [ style "max-width" "40rem" ]
            [ descriptionInput EditedDescription form.description
            ]
        ]


viewLapFields : ActivityConfigs -> ActivityForm -> Html Msg
viewLapFields { emojis, levelM } form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column
        [ style "justify-content" "space-between"
        , style "max-height" "25rem"
        , style "flex-grow" "5"
        , style "padding-bottom" "1rem"
        ]
        [ row []
            [ column [ maxFieldWidth ] [ Actions.viewLapActions True ] ]
        , row []
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ activityTypeSelect SelectedActivityType form.activityType ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ repeatsInput EditedRepeats form.repeats form.validated.repeats ]
            ]
        , row []
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ durationInput EditedDuration form.duration ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ effortSelect SelectedEffort form.effort ]
            ]
        , row []
            [ column [ maxFieldWidth, style "flex-grow" "2", styleIf (form.activityType /= Activity.Types.Run) "visibility" "hidden" ] [ distanceInput EditedDistance form.distance form.distanceUnits form.validated.distance ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ emojiSelect SelectedEmoji emojis form.emoji form.emojiSearch ]
            ]
        , row [ styleIf (form.activityType /= Activity.Types.Run) "visibility" "hidden" ]
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ paceSelect levelM SelectedPace form.pace form.validated.pace ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ raceToggle CheckedRace form.race ]
            ]
        ]


toActivityData : ActivityForm -> ActivityData
toActivityData model =
    { activityType = model.activityType
    , duration =
        case model.duration of
            ( "", "", "" ) ->
                Nothing

            _ ->
                model.validated.duration |> Result.toMaybe
    , completed = model.completed
    , pace =
        if model.activityType == Activity.Types.Run then
            model.validated.pace |> Result.toMaybe

        else
            Nothing
    , distance =
        if model.activityType == Activity.Types.Run then
            model.validated.distance
                |> Result.toMaybe
                |> Maybe.map (Distance.toMeters model.distanceUnits)

        else
            Nothing
    , distanceUnits =
        if model.activityType == Activity.Types.Run then
            Just model.distanceUnits

        else
            Nothing
    , race =
        if model.activityType == Activity.Types.Run then
            model.race

        else
            Nothing
    , effort = model.effort
    , emoji =
        case model.emoji of
            "" ->
                Nothing

            _ ->
                Just model.emoji
    }


label : String -> Bool -> Msg -> Html Msg
label name showClear onClear =
    row []
        [ Html.label
            [ style "color" "var(--black-500)"
            , style "font-size" "0.8rem"
            ]
            [ text name
            ]
        , viewIf showClear
            (compactColumn
                [ style "margin-left" "0.2rem"
                , style "cursor" "pointer"
                , style "margin-top" "-2px"
                , style "margin-bottom" "-2px"
                , onClick onClear
                ]
                [ MonoIcons.icon (MonoIcons.close "var(--grey-900)")
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
            , style "margin-top" "3px"
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
                , styleIf (activityType == aType) "border" "1px solid var(--blue-300)"
                ]
    in
    column [ style "margin-right" "1rem" ]
        [ label "Type" False NoOp
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
        [ label "Date" False NoOp
        , row []
            [ button [ class "button", onClick msg ]
                [ text (Maybe.map (Date.format "E MMM d") date |> Maybe.withDefault "Select Date")
                ]
            ]
        ]


completionToggle : Msg -> Activity.Types.Completion -> Html Msg
completionToggle msg completed =
    column []
        [ row [ class "button-group" ]
            [ Html.button
                [ class "button medium"
                , style "text-align" "center"
                , attributeIf (completed == Activity.Types.Completed) (onClick msg)
                , styleIf (completed == Activity.Types.Planned) "background-color" "var(--grey-500)"
                ]
                [ text "Planned" ]
            , Html.button
                [ class "button medium"
                , style "text-align" "center"
                , attributeIf (completed == Activity.Types.Planned) (onClick msg)
                , styleIf (completed == Activity.Types.Completed) "background-color" "var(--grey-500)"
                ]
                [ text "Completed" ]
            ]
        ]


repeatsInput : (String -> Msg) -> Maybe String -> Result FieldError Int -> Html Msg
repeatsInput msg countStrM result =
    column [ style "max-width" "6rem" ]
        [ label "Repeats" (countStrM /= Just "" && countStrM /= Nothing) ClickedRepeat
        , case countStrM of
            Just countStr ->
                row []
                    [ numberInput "repeats"
                        99
                        [ onInput msg
                        , value countStr
                        , style "width" "2.5rem"
                        ]
                        []
                    ]

            Nothing ->
                row [] [ squareIconButton (MonoIcons.repeat "#3d3d3d") "Repeat" [ onClick ClickedRepeat ] ]
        ]


emojiSelect : (String -> Msg) -> EmojiDict -> String -> String -> Html Msg
emojiSelect msg emojis name search =
    let
        results =
            if String.isEmpty search then
                Emoji.recommended emojis

            else
                Emoji.filter (Emoji.toList emojis) (String.toLower search)

        emojiItem data =
            compactColumn
                [ onClick (msg data.name)
                , style "border-radius" "11px"
                , style "width" "22px"
                , style "height" "22px"
                , style "margin-right" "10px"
                , style "margin-bottom" "5px"
                , style "cursor" "pointer"
                , styleIf (data.name == name) "box-shadow" "0 0 0 0.2rem var(--grey-900)"
                ]
                [ Emoji.view data ]
    in
    column [ style "width" "100px" ]
        [ row [ style "justify-content" "space-between" ]
            [ label "Feel" (name /= "") (msg "")
            , compactColumn [ borderStyle "border-bottom" ]
                [ row []
                    [ MonoIcons.icon (MonoIcons.search "var(--grey-900)")
                    , input
                        [ onInput SearchedEmojis
                        , onFocus (SearchedEmojis "")
                        , style "width" "2rem"
                        , style "padding" "0"
                        , style "border" "none"
                        , value search
                        ]
                        []
                    ]
                ]
            ]
        , row [ style "margin-top" "4px", style "height" "55px", style "flex-wrap" "wrap" ]
            (List.map emojiItem (results |> List.take 6))
        ]


durationInput : (( String, String, String ) -> Msg) -> ( String, String, String ) -> Html Msg
durationInput msg ( hrs, mins, secs ) =
    let
        header str =
            row [ style "font-size" "0.6rem", style "color" "var(--grey-900)", style "margin-bottom" "2px" ]
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
                    , style "border-top-right-radius" "0"
                    , style "border-bottom-right-radius" "0"
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "MINS"
                , numberInput "minutes"
                    60
                    [ onInput (\m -> msg ( hrs, m, secs ))
                    , value mins
                    , style "border-top-left-radius" "0"
                    , style "border-bottom-left-radius" "0"
                    , style "border-top-right-radius" "0"
                    , style "border-bottom-right-radius" "0"
                    ]
                    []
                ]
            , compactColumn [ style "width" "3.5rem" ]
                [ header "SECS"
                , numberInput "seconds"
                    60
                    [ onInput (\s -> msg ( hrs, mins, s ))
                    , value secs
                    , style "border-top-left-radius" "0"
                    , style "border-bottom-left-radius" "0"
                    ]
                    []
                ]
            ]
        ]


distanceInput : (String -> Msg) -> String -> DistanceUnits -> Result FieldError Float -> Html Msg
distanceInput msg dist units result =
    let
        eventDecoder =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map Activity.distanceUnits.fromString
                |> Decode.map (Maybe.withDefault Activity.Types.Miles)
                |> Decode.map SelectedDistanceUnits
    in
    column []
        [ label "Distance" (dist /= "") (msg "")
        , row []
            [ numberInput "distance"
                100000
                [ onInput msg
                , case ( dist, result ) of
                    ( "", Ok distance ) ->
                        Html.Attributes.placeholder (String.fromFloat distance)

                    _ ->
                        Html.Attributes.placeholder ""
                , value dist
                , style "width" "4rem"
                , style "border-top-right-radius" "0"
                , style "border-bottom-right-radius" "0"
                ]
                []
            , Html.select
                [ style "border-top-left-radius" "0"
                , style "border-bottom-left-radius" "0"
                , style "border-left-width" "0"
                , Html.Events.on "change" eventDecoder
                ]
                (List.map
                    (\( unitStr, unitOpt ) ->
                        Html.option
                            [ Html.Attributes.value unitStr
                            , Html.Attributes.selected (unitOpt == units)
                            ]
                            [ Html.text unitStr ]
                    )
                    Activity.distanceUnits.list
                )
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
         , Html.Attributes.id nameStr
         , Html.Attributes.attribute "aria-label" nameStr
         ]
            ++ attrs
        )


paceSelect : Maybe Int -> (String -> Msg) -> String -> Result FieldError Int -> Html Msg
paceSelect levelM msg paceStr result =
    let
        trainingPaces =
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
                                    [ style "background-color" "var(--green-500)"
                                    , onClick (msg time)
                                    , style "height" "0.5rem"
                                    , style "margin-right" "1px"
                                    , style "cursor" "pointer"
                                    , styleIf (isSlowerThan time)
                                        "background-color"
                                        "var(--grey-300)"
                                    ]
                                    []
                            )
                            trainingPaces
                        )
                )
            , row []
                [ input
                    [ onInput msg
                    , class "input"
                    , style "width" "3rem"
                    , value paceStr
                    , case ( paceStr, result ) of
                        ( "", Ok pace ) ->
                            Html.Attributes.placeholder (Pace.paceToString pace)

                        _ ->
                            Html.Attributes.placeholder "mm:ss"
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
        , row [ style "margin-top" "0.6rem" ]
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


raceToggle : Msg -> Maybe Activity.Types.RaceDistance -> Html Msg
raceToggle msg raceM =
    column []
        [ label "Race" False NoOp
        , Html.input
            [ onClick msg
            , Html.Attributes.attribute "type" "checkbox"
            , style "width" "1.5rem"
            , style "height" "1.5rem"
            , attributeIf (raceM /= Nothing) (Html.Attributes.attribute "checked" "")
            ]
            []
        ]
