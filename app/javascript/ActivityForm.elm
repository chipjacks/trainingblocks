module ActivityForm exposing (init, initMove, update, view)

import Actions exposing (viewFormActions)
import Activity
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, ActivityType, DistanceUnits(..), LapData(..))
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
    , laps = laps
    , repeat = newRepeatM
    , validated = Validate.init
    , date = Just activity.date
    , description = activity.description
    , repeats = Maybe.map String.fromInt countM
    , activityType = data.activityType
    , duration = duration
    , completed = data.completed
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
            ( updateLaps (\_ laps -> Selection.add (Individual newData) laps) model
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
                |> updateFromSelection
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
        selectedLap =
            Selection.get model.laps
                |> Maybe.withDefault (Individual Activity.initActivityData)

        newLaps =
            transform selectedLap model.laps
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
    initFromSelection model.activity model.laps model.repeat


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
            Activity.Laps.set activity (Selection.toList laps)

        updateActivityDescription description activity =
            { activity | description = description }
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

                    ( _, repeatM ) ->
                        ( Selection.set (Individual <| toActivityData m) m.laps, repeatM )
            )
        |> (\m ->
                { m
                    | activity =
                        updateActivityLaps m.laps m.activity
                            |> updateActivityDescription m.description
                }
           )


view : ActivityConfigs -> ActivityState -> Html Msg
view configs activityM =
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
        Editing model ->
            if model.date /= Nothing then
                row (openAttributes "100%")
                    [ column []
                        [ row []
                            [ column [ style "margin-bottom" "1rem" ]
                                [ viewFormActions ]
                            ]
                        , row []
                            [ viewActivityFields configs.emojis model ]
                        , row [ style "margin-top" "10px", style "margin-bottom" "5px", borderStyle "border-bottom" ]
                            []
                        , expandingRow [ style "overflow" "hidden" ]
                            [ viewLapShapes configs model.laps model.repeat
                            , viewLapFields configs.levelM model
                            ]
                        ]
                    ]

            else
                row (openAttributes "1.5rem")
                    [ row []
                        [ MonoIcons.icon (MonoIcons.circleInformation "var(--blue-500)")
                        , column [ style "margin-left" "1rem" ] [ text "Select Date" ]
                        ]
                    ]

        _ ->
            row closedAttributes []


viewLapShapes : ActivityConfigs -> Selection LapData -> Maybe (Selection ActivityData) -> Html Msg
viewLapShapes configs lapSelection repeatSelectionM =
    column
        [ style "min-width" "4rem"
        , style "overflow-y" "scroll"
        , class "hide-scrollbars"
        , style "padding-left" "3px"
        , style "flex-grow" "1"
        ]
        (List.concat
            [ Selection.toList lapSelection
                |> List.indexedMap (\i lap -> viewActivityShape configs (Selection.selectedIndex lapSelection) i lap repeatSelectionM)
                |> List.concat
            , [ row [ style "padding-top" "0.5rem", style "padding-bottom" "0.5rem" ] [ viewAddButton ClickedAddLap ] ]
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
                            , style "padding-top" "0.5rem"
                            , style "padding-left" "0.5rem"
                            , borderStyle "border-left"
                            , style "padding-bottom" "0.5rem"
                            , style "min-height" "1rem"
                            ]
                            [ ActivityShape.view configs data ]
                    )
                    (Selection.toList repeat)
                , [ row
                        [ style "padding-top" "0.5rem"
                        , style "padding-left" "0.5rem"
                        , borderStyle "border-left"
                        , style "padding-bottom" "0.5rem"
                        ]
                        [ viewAddButton ClickedAddRepeat ]
                  ]
                , List.map
                    (\data ->
                        row
                            [ style "opacity" "0.5"
                            , style "padding-left" "0.5rem"
                            , borderStyle "border-left"
                            ]
                            [ ActivityShape.view configs data ]
                    )
                    (List.repeat (count - 1) list |> List.concat)
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
                            [ ActivityShape.view configs data ]

                Repeats count list ->
                    [ row [ style "padding-top" "0.5rem" ] [] ]
                        ++ (List.map (\data -> row [ onClick (SelectedLap lapIndex) ] [ ActivityShape.view configs data ]) list
                                |> List.repeat count
                                |> List.concat
                           )
                        ++ [ row [ style "padding-bottom" "0.5rem" ] [] ]


viewAddButton : Msg -> Html Msg
viewAddButton msg =
    row []
        [ iconButton [ onClick msg ] [ MonoIcons.icon (MonoIcons.add "var(--grey-900)") ]
        ]


viewActivityFields : EmojiDict -> ActivityForm -> Html Msg
viewActivityFields emojis form =
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
            , column [ maxFieldWidth ] [ emojiSelect SelectedEmoji emojis form.emoji form.emojiSearch ]
            ]
        ]


viewLapFields : Maybe Int -> ActivityForm -> Html Msg
viewLapFields levelM form =
    let
        maxFieldWidth =
            style "max-width" "20rem"
    in
    column
        [ style "justify-content" "space-between"
        , style "max-height" "25rem"
        , style "margin-bottom" "10px"
        , style "margin-top" "10px"
        , style "flex-grow" "5"
        ]
        [ row []
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ activityTypeSelect SelectedActivityType form.activityType ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ repeatsInput EditedRepeats form.repeats form.validated.repeats ]
            ]
        , row []
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ durationInput EditedDuration form.duration ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ effortSelect SelectedEffort form.effort ]
            ]
        , row [ styleIf (form.activityType /= Activity.Types.Run) "visibility" "hidden" ]
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ distanceInput EditedDistance form.distance form.distanceUnits form.validated.distance ]
            , column [ maxFieldWidth, style "flex-grow" "1" ] [ raceToggle CheckedRace form.race ]
            ]
        , row [ styleIf (form.activityType /= Activity.Types.Run) "visibility" "hidden" ]
            [ column [ maxFieldWidth, style "flex-grow" "2" ] [ paceSelect levelM SelectedPace form.pace form.validated.pace ]

            --, column [ maxFieldWidth, style "flex-grow" "1" ] [ raceSelect SelectedRace form.race ]
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
    column []
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
        [ label "Completed" False NoOp
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
        [ label "Repeats" (countStrM /= Just "" && countStrM /= Nothing) ClickedRepeat
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
    column [ style "width" "150px" ]
        [ row [ style "justify-content" "space-between" ]
            [ label "Feel" (name /= "") (msg "")
            , compactColumn [ borderStyle "border-bottom" ]
                [ row []
                    [ MonoIcons.icon (MonoIcons.search "var(--grey-900)")
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
            (List.map emojiItem (results |> List.take 5))
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
