module Page.Calendar exposing (Model, init, main, update, view)

import Activity
import Activity.Laps
import Activity.Types exposing (Activity, Completion(..), LapData(..))
import ActivityForm
import ActivityShape
import Api
import App
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Calendar
import Date exposing (Date)
import Dict
import Effect exposing (Effect)
import Emoji exposing (EmojiDict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, id, style)
import Html.Events
import Html.Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import MPRLevel
import MonoIcons
import Msg exposing (ActivityConfigs, ActivityState(..), Msg(..))
import Pace
import Pace.List exposing (PaceList)
import Ports
import Random
import Report
import Store
import Task
import Time
import UI exposing (spinner)
import UI.Layout exposing (compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import UI.Toast
import UI.Util exposing (onPointerMove, viewMaybe)



-- INIT


main : Program App.Flags ( App.Env, Model ) Msg
main =
    App.document
        { title = "Calendar"
        , init = init
        , update = update
        , perform = Effect.perform
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = Loading { todayM : Maybe Date, storeM : Maybe Store.Model, emojisM : Maybe EmojiDict, levelM : Maybe Int, pacesM : Maybe (PaceList String) }
    | Loaded State
    | MissingSettings
    | Error String


type State
    = State Calendar.Model Store.Model ActivityState ActivityConfigs


init : ( Model, Effect )
init =
    ( Loading { todayM = Nothing, storeM = Nothing, emojisM = Nothing, levelM = Nothing, pacesM = Nothing }
    , Effect.Batch
        [ Effect.DateToday Jump
        , Effect.GetActivities
        , Effect.FetchEmojis
        , Effect.GetSettings
        ]
    )



-- UPDATING MODEL


update : App.Env -> Msg -> Model -> ( Model, Effect )
update env msg model =
    case model of
        Loading state ->
            case msg of
                Jump date ->
                    Loading { state | todayM = Just date }
                        |> updateLoading

                GotActivities activitiesR ->
                    case activitiesR of
                        Ok ( revision, activities ) ->
                            Loading { state | storeM = Just (Store.init revision activities) }
                                |> updateLoading

                        Err err ->
                            ( Error (Api.userError err)
                            , reportError env "Loading" "GotActivities" (Api.developerError err)
                            )

                GotSettings settingsR ->
                    case settingsR of
                        Ok (Just settings) ->
                            Loading { state | pacesM = Just settings.paces, levelM = Just settings.level }
                                |> updateLoading

                        Ok Nothing ->
                            ( MissingSettings, Effect.None )

                        Err err ->
                            ( Error (Api.userError err)
                            , reportError env "Loading" "GotSettings" (Api.developerError err)
                            )

                FetchedEmojis result ->
                    case result of
                        Ok emojis ->
                            Loading { state | emojisM = Just (Emoji.toDict emojis) }
                                |> updateLoading

                        Err err ->
                            ( model
                            , reportError env "Loading" "FetchedEmojis" (Api.developerError err)
                            )

                _ ->
                    ( model, Effect.None )

        Error _ ->
            ( model, Effect.None )

        MissingSettings ->
            ( model, Effect.None )

        Loaded state ->
            let
                (State calendar store activityM configs) =
                    state
            in
            case msg of
                GotActivities _ ->
                    updateStore msg state
                        |> Tuple.mapFirst reloadActivityState
                        |> loaded

                FetchedEmojis _ ->
                    ( model, Effect.None )

                GotSettings _ ->
                    ( model, Effect.None )

                VisibilityChange visibility ->
                    case visibility of
                        Events.Visible ->
                            ( model, Effect.Batch [ Effect.DateToday LoadToday, Effect.GetActivities ] )

                        Events.Hidden ->
                            updateStore FlushNow state
                                |> loaded

                KeyPressed key ->
                    case key of
                        "Enter" ->
                            updateActivityForm ClickedSubmit state
                                |> loaded

                        "Escape" ->
                            updateActivityState None state |> loaded

                        _ ->
                            ( model, Effect.None )

                MouseMoved x y ->
                    let
                        newActivityM =
                            case activityM of
                                Moving activity _ _ ->
                                    Moving activity x y

                                _ ->
                                    activityM
                    in
                    updateActivityState newActivityM state |> loaded

                AutoScrollCalendar y ->
                    let
                        distance =
                            50

                        navbarHeight =
                            50

                        autoScrollCalendar =
                            Dom.getViewportOf "main"
                                |> Task.andThen
                                    (\info ->
                                        if y < 0 then
                                            Task.succeed ()

                                        else if y > (info.viewport.height * 0.9 + navbarHeight) then
                                            Dom.setViewportOf "main" 0 (info.viewport.y + distance)

                                        else if y < (info.viewport.height * 0.1 + navbarHeight) then
                                            Dom.setViewportOf "main" 0 (info.viewport.y - distance)

                                        else
                                            Task.succeed ()
                                    )
                    in
                    ( model, Effect.Cmd (Task.attempt (\_ -> NoOp) autoScrollCalendar) )

                MouseReleased ->
                    let
                        newActivityM =
                            case activityM of
                                Moving activity _ _ ->
                                    Selected [ activity ]

                                _ ->
                                    activityM
                    in
                    updateActivityState newActivityM state |> loaded

                MoveTo date ->
                    case activityM of
                        Moving activity x y ->
                            if activity.date == date then
                                ( model, Effect.None )

                            else
                                let
                                    newActivityM =
                                        Moving { activity | date = date } x y
                                in
                                updateStore (Move date activity) (State calendar store newActivityM configs) |> loaded

                        _ ->
                            ( model, Effect.None )

                ReportedError result ->
                    ( model, Effect.None )

                NoOp ->
                    ( model, Effect.None )

                Create activity ->
                    updateActivityState (Selected [ activity ]) state
                        |> Tuple.first
                        |> updateStore msg
                        |> loaded

                Group _ session ->
                    updateActivityState (Selected [ session ]) state
                        |> Tuple.first
                        |> updateStore msg
                        |> loaded

                Ungroup activities _ ->
                    updateActivityState (Selected activities) state
                        |> Tuple.first
                        |> updateStore msg
                        |> loaded

                Update activity ->
                    updateActivityState (Selected [ activity ]) state
                        |> Tuple.first
                        |> updateStore msg
                        |> loaded

                Move _ _ ->
                    updateStore msg state |> loaded

                Shift _ _ ->
                    updateStore msg state |> loaded

                Delete _ ->
                    updateActivityState None state
                        |> Tuple.first
                        |> updateStore msg
                        |> loaded

                Undo _ ->
                    updateStore msg state
                        |> Tuple.mapFirst reloadActivityState
                        |> loaded

                Posted _ _ ->
                    updateStore msg state |> loaded

                DebounceFlush _ ->
                    updateStore msg state |> loaded

                FlushNow ->
                    updateStore msg state |> loaded

                LoadToday _ ->
                    updateCalendar msg state
                        |> loaded

                Jump _ ->
                    updateCalendar msg state
                        |> loaded

                ChangeZoom _ dateM ->
                    let
                        ( calendarState, calendarCmd ) =
                            updateCalendar msg state

                        ( activityFormState, activityFormCmd ) =
                            updateActivityForm (Maybe.map SelectedDate dateM |> Maybe.withDefault NoOp) calendarState
                    in
                    ( activityFormState, Effect.Batch [ calendarCmd, activityFormCmd ] )
                        |> loaded

                Scroll _ _ _ ->
                    updateCalendar msg state
                        |> loaded

                ScrollCompleted ->
                    updateCalendar msg state
                        |> loaded

                ReceiveSelectDate _ ->
                    updateCalendar msg state
                        |> loaded

                ClickedNewActivity date ->
                    ( model, initActivity (calendar |> Calendar.get |> .today) (Just date) )

                NewActivity activity ->
                    let
                        form =
                            ActivityForm.init activity
                    in
                    updateStore (Create activity) (State calendar store (Editing form) configs)
                        |> loaded

                EditActivity activity ->
                    let
                        form =
                            ActivityForm.init activity
                    in
                    updateActivityState (Editing form) state
                        |> loaded

                SelectActivity activity shiftKey ->
                    case ( activityM, shiftKey ) of
                        ( Selected selected, True ) ->
                            let
                                indexedActivities =
                                    List.filter (\a -> a.date == activity.date) (Store.get store .activities)
                                        |> List.indexedMap Tuple.pair

                                range =
                                    indexedActivities
                                        |> List.filter (\( _, a ) -> (activity :: selected) |> List.map .id |> List.member a.id)
                                        |> List.unzip
                                        |> Tuple.first
                                        |> (\indexes -> ( List.minimum indexes, List.maximum indexes ))

                                list =
                                    case range of
                                        ( Just start, Just end ) ->
                                            indexedActivities
                                                |> List.filter (\( i, _ ) -> i >= start && i <= end)
                                                |> List.unzip
                                                |> Tuple.second

                                        _ ->
                                            [ activity ]
                            in
                            updateActivityState (Selected list) state |> loaded

                        _ ->
                            updateActivityState (Selected [ activity ]) state |> loaded

                MoveActivity activity ->
                    ( Loaded <| State calendar store (Moving activity -100 -100) configs, Effect.None )

                SelectedDate _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedDescription _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedLap _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedRepeatLap _ ->
                    updateActivityForm msg state
                        |> loaded

                ClickedAddLap ->
                    updateActivityForm msg state
                        |> loaded

                ClickedAddRepeat ->
                    updateActivityForm msg state
                        |> loaded

                SearchedEmojis _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedEmoji _ ->
                    updateActivityForm msg state
                        |> loaded

                CheckedCompleted ->
                    updateActivityForm msg state
                        |> loaded

                EditedRepeats _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedActivityType _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedDuration _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedDistance _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedDistanceUnits _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedPace _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedPace _ ->
                    updateActivityForm msg state
                        |> loaded

                CheckedRace ->
                    updateActivityForm msg state
                        |> loaded

                SelectedEffort _ ->
                    updateActivityForm msg state
                        |> loaded

                ClickedAutofill ->
                    updateActivityForm msg state
                        |> loaded

                ClickedClearLaps ->
                    updateActivityForm msg state
                        |> loaded

                ClickedSubmit ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        _ ->
                            updateActivityState None state
                                |> loaded

                ClickedEdit ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        Selected [ activity ] ->
                            let
                                form =
                                    ActivityForm.init activity
                            in
                            updateActivityState (Editing form) state
                                |> loaded

                        _ ->
                            ( model, Effect.None )

                ClickedCopy ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        Selected [ activity ] ->
                            ( model
                            , Activity.newId
                                |> Random.map (\id -> { activity | id = id })
                                |> Random.generate Create
                                |> Effect.Cmd
                            )

                        _ ->
                            ( model, Effect.None )

                ClickedRepeat ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        _ ->
                            ( model, Effect.None )

                ClickedDelete ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        Selected list ->
                            updateActivityState None state
                                |> loaded
                                |> Tuple.mapSecond
                                    (\_ ->
                                        Effect.Batch (List.map (\a -> Store.cmd (Delete a)) list)
                                    )

                        _ ->
                            ( model, Effect.None )

                ClickedShift up ->
                    case activityM of
                        Editing _ ->
                            updateActivityForm msg state
                                |> loaded

                        Selected [ activity ] ->
                            ( model
                            , Store.cmd (Shift up activity)
                            )

                        _ ->
                            ( model, Effect.None )

                ClickedMove ->
                    let
                        ( calendarState, calendarCmd ) =
                            updateCalendar (ChangeZoom Msg.Year Nothing) state

                        ( activityFormState, activityFormCmd ) =
                            updateActivityForm msg calendarState
                    in
                    ( activityFormState, Effect.Batch [ calendarCmd, activityFormCmd ] )
                        |> loaded

                ClickedGroup ->
                    case activityM of
                        Selected (a :: tail) ->
                            ( model, initSession a (a :: tail) )

                        _ ->
                            ( model, Effect.None )

                ClickedClose ->
                    updateActivityState None state
                        |> loaded

                NewId _ ->
                    updateActivityForm msg state
                        |> loaded


updateLoading : Model -> ( Model, Effect )
updateLoading model =
    case model of
        Loading { todayM, storeM, emojisM, levelM, pacesM } ->
            Maybe.map5
                (\today store emojis level customPaces ->
                    let
                        ( calendarModel, calendarEffect ) =
                            Calendar.init Msg.Month today today

                        paces =
                            Pace.standardPaces ( MPRLevel.Neutral, level )
                    in
                    ( Loaded (State calendarModel store None (ActivityConfigs paces customPaces emojis))
                    , calendarEffect
                    )
                )
                todayM
                storeM
                emojisM
                levelM
                pacesM
                |> Maybe.withDefault ( model, Effect.None )

        _ ->
            ( model, Effect.None )


updateActivityForm : Msg -> State -> ( State, Effect )
updateActivityForm msg (State calendar store activityM configsM) =
    let
        ( newActivityM, cmd ) =
            case activityM of
                Editing form ->
                    ActivityForm.update msg form |> Tuple.mapFirst Editing

                _ ->
                    ( activityM, Effect.None )
    in
    ( State calendar store newActivityM configsM, cmd )


updateCalendar : Msg -> State -> ( State, Effect )
updateCalendar msg (State calendar store activityM configs) =
    Calendar.update msg calendar
        |> Tuple.mapFirst (\updated -> State updated store activityM configs)


updateStore : Msg -> State -> ( State, Effect )
updateStore msg (State calendar store activityM configs) =
    Store.update msg store
        |> Tuple.mapFirst (\updated -> State calendar updated activityM configs)


updateActivityState : ActivityState -> State -> ( State, Effect )
updateActivityState newActivityM (State calendar store _ configs) =
    ( State calendar store newActivityM configs, Effect.None )


reloadActivityState : State -> State
reloadActivityState (State calendar store activityM configs) =
    let
        reloadActivity activity =
            Store.get store .activities
                |> List.filter (\a -> a.id == activity.id)
                |> List.head

        newActivityM =
            case activityM of
                Editing form ->
                    reloadActivity form.activity
                        |> Maybe.map ActivityForm.init
                        |> Maybe.map Editing
                        |> Maybe.withDefault None

                Selected [ activity ] ->
                    reloadActivity activity
                        |> Maybe.map (\a -> Selected [ a ])
                        |> Maybe.withDefault None

                _ ->
                    None
    in
    State calendar store newActivityM configs


loaded : ( State, Effect ) -> ( Model, Effect )
loaded stateTuple =
    Tuple.mapFirst Loaded stateTuple


initActivity : Date -> Maybe Date -> Effect
initActivity today dateM =
    let
        date =
            dateM |> Maybe.withDefault today

        ( laps, planned ) =
            if Date.compare date today == LT || date == today then
                ( [ Individual { activityData | completed = Completed } ]
                , []
                )

            else
                ( []
                , [ Individual { activityData | completed = Planned } ]
                )

        activityData =
            Activity.initActivityData
    in
    Activity.newId
        |> Random.map
            (\id ->
                Activity id
                    date
                    ""
                    laps
                    planned
            )
        |> Effect.GenerateActivity NewActivity


initSession : Activity -> List Activity -> Effect
initSession head activities =
    Activity.newId
        |> Random.map
            (\id ->
                Activity id
                    head.date
                    ""
                    (List.concatMap .laps activities)
                    (List.concatMap .planned activities)
            )
        |> Effect.GenerateActivity (Group activities)



-- VIEW


view : Model -> Html Msg
view model =
    let
        withBody skeleton =
            case model of
                Loading _ ->
                    Skeleton.withBody (viewNotice (spinner "3rem") []) skeleton

                Error errorString ->
                    Skeleton.withBody (viewErrorNotice errorString) skeleton

                MissingSettings ->
                    Skeleton.withBody viewMissingSettingsNotice skeleton

                Loaded state ->
                    Skeleton.withContainer identity skeleton
                        |> Skeleton.withBody
                            (viewBody state)
    in
    Skeleton.default
        |> Skeleton.withNavbar (viewNavbar model)
        |> withBody
        |> Skeleton.view


viewMissingSettingsNotice : Html msg
viewMissingSettingsNotice =
    viewNotice (MonoIcons.icon (MonoIcons.settings "var(--grey-900)"))
        [ text "Please configure your"
        , a [ href "/settings", style "margin-left" "0.5rem", style "margin-right" "0.5rem" ] [ text "settings" ]
        , text "before continuing."
        ]


viewErrorNotice : String -> Html msg
viewErrorNotice str =
    viewNotice (MonoIcons.icon (MonoIcons.warning "var(--grey-900)")) [ text str ]


viewNotice : Html msg -> List (Html msg) -> Html msg
viewNotice icon message =
    expandingRow
        [ style "justify-content" "center", style "align-items" "center", style "padding-top" "2rem" ]
        [ compactColumn [ style "width" "300px", style "font-size" "3rem", style "align-items" "center" ]
            [ icon
            , div [ style "font-size" "1.5rem", style "text-align" "center" ]
                message
            ]
        ]


viewBody : State -> Html Msg
viewBody (State calendar store activityM configs) =
    let
        activities =
            Store.get store .activities

        events =
            case activityM of
                Moving _ _ _ ->
                    [ onPointerMove MouseMoved
                    , Html.Events.on "pointerup" (Decode.succeed MouseReleased)
                    , style "touch-action" "none"
                    , style "pointer-action" "none"
                    , style "cursor" "grabbing"
                    ]

                _ ->
                    []

        ( activeId, isMoving ) =
            case activityM of
                Selected list ->
                    ( List.map .id list |> String.join " ", False )

                Editing _ ->
                    ( "", False )

                Moving { id } _ _ ->
                    ( id, True )

                None ->
                    ( "", False )

        activeRataDie =
            case activityM of
                Editing { date } ->
                    Maybe.map Date.toRataDie date |> Maybe.withDefault 0

                Selected (a :: _) ->
                    Date.toRataDie a.date

                _ ->
                    0
    in
    expandingRow
        ([ id "main"
         , style "overflow-y" "scroll"
         , Calendar.handleScroll calendar
         ]
            ++ events
        )
        [ Html.Lazy.lazy6 Calendar.view calendar activities activeId activeRataDie isMoving configs
        , Html.Lazy.lazy2 viewActivityM configs activityM
        , Html.Lazy.lazy2 ActivityForm.view configs activityM
        , Html.Lazy.lazy viewUndoToastM (Store.undoMsg store)
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    let
        loading storeM =
            Maybe.map Store.needsFlush storeM |> Maybe.withDefault False
    in
    case model of
        Loaded (State calendar store _ _) ->
            Navbar.default
                |> Navbar.withLoading (loading (Just store))
                |> Navbar.withBackButton (Calendar.viewBackButton calendar)
                |> Navbar.withItems (Calendar.viewMenu calendar)
                |> Navbar.withSecondRow (Html.Lazy.lazy Calendar.viewHeader calendar)
                |> Navbar.view

        _ ->
            Navbar.default
                |> Navbar.view


viewActivityM : ActivityConfigs -> ActivityState -> Html Msg
viewActivityM configs activityState =
    case activityState of
        Moving activity x y ->
            row
                [ style "position" "fixed"
                , style "left" (String.fromFloat x ++ "px")
                , style "top" (String.fromFloat y ++ "px")
                , style "z-index" "3"
                , style "margin-left" "10px"
                , style "margin-top" "10px"
                ]
                [ compactColumn [ style "flex-basis" "5rem" ]
                    (Activity.Laps.listData activity
                        |> List.map (\lap -> ActivityShape.view configs lap)
                    )
                ]

        _ ->
            Html.text ""


viewUndoToastM : Maybe ( Int, String, Msg ) -> Html Msg
viewUndoToastM eventM =
    viewMaybe eventM
        (\( stackHeight, name, msg ) ->
            UI.Toast.bottom
                |> UI.Toast.withStackHeight stackHeight
                |> UI.Toast.view
                    (row []
                        [ text name
                        , Html.a
                            [ Html.Events.onClick msg
                            , style "margin-left" "0.5rem"
                            ]
                            [ text "Undo" ]
                        ]
                    )
        )



-- SUBSCRIPTIONS


keyPressDecoder : Decode.Decoder Msg
keyPressDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded (State _ _ activityM _) ->
            Sub.batch
                [ Ports.selectDateFromScroll ReceiveSelectDate
                , Events.onVisibilityChange VisibilityChange
                , case activityM of
                    Editing _ ->
                        Events.onKeyDown keyPressDecoder

                    Selected _ ->
                        Events.onKeyDown keyPressDecoder

                    Moving _ _ y ->
                        Time.every 100 (\_ -> AutoScrollCalendar y)

                    _ ->
                        Sub.none
                ]

        _ ->
            Sub.none



-- ERROR REPORTING


reportError : App.Env -> String -> String -> String -> Effect
reportError env model msg errorMsg =
    Report.error env
        |> Report.withField "model" (Encode.string model)
        |> Report.withField "msg" (Encode.string msg)
        |> Report.send errorMsg
        |> Effect.ReportError
