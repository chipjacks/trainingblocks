module Main exposing (Model, init, main, update, view)

import Activity exposing (Activity)
import ActivityForm
import ActivityShape
import Api
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Calendar
import Date exposing (Date)
import Effect exposing (Effect)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (on)
import Html.Lazy
import Json.Decode as Decode
import MonoIcons
import Msg exposing (ActivityForm, ActivityState(..), Msg(..))
import Ports
import Random
import Skeleton exposing (borderStyle, column, compactColumn, expandingRow, row, spinner, styleIf, viewIf, viewMaybe)
import Store
import Task
import Time



-- INIT


main =
    Browser.document
        { init = \x -> init x |> Tuple.mapSecond Effect.perform
        , view = \model -> { title = "Rhino Log", body = [ Skeleton.layout (viewNavbar model) (view model) ] }
        , update = \model msg -> update model msg |> Tuple.mapSecond Effect.perform
        , subscriptions = subscriptions
        }


type Model
    = Loading (Maybe Date) (Maybe Store.Model) String
    | Loaded State
    | Error String


type State
    = State Calendar.Model Store.Model ActivityState


init : { csrfToken : String } -> ( Model, Effect )
init { csrfToken } =
    ( Loading Nothing Nothing csrfToken
    , Effect.Batch
        [ Effect.DateToday Jump
        , Effect.GetActivities
        ]
    )


viewNavbar : Model -> Html Msg
viewNavbar model =
    case model of
        Loaded (State calendar store activityState) ->
            row
                [ style "padding" "0.5rem" ]
                [ case activityState of
                    Editing { date } ->
                        column [] [ Calendar.viewMenu (date /= Nothing) calendar ]

                    _ ->
                        column [] [ Calendar.viewMenu False calendar ]
                , compactColumn [ style "min-width" "1.5rem", style "justify-content" "center" ]
                    [ div [ class "dropdown" ]
                        [ if Store.needsFlush store then
                            spinner "1.5rem"

                          else
                            div [ style "font-size" "1.4rem", style "padding-top" "2px" ] [ MonoIcons.icon (MonoIcons.optionsVertical "var(--icon-gray)") ]
                        , div [ class "dropdown-content", style "right" "0" ]
                            [ a [ Html.Attributes.href " /users/sign_out", Html.Attributes.attribute "data-method" "delete" ] [ text "Logout" ] ]
                        ]
                    ]
                ]

        _ ->
            row [ style "padding" "0.5rem" ]
                [ compactColumn [ style "justify-content" "center" ] [ Skeleton.logo ]
                , column [] []
                , compactColumn [ style "justify-content" "center" ] [ spinner "1.5rem" ]
                ]



-- UPDATING MODEL


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case model of
        Loading dateM activitiesM token ->
            case msg of
                Jump date ->
                    Loading (Just date) activitiesM token
                        |> updateLoading

                GotActivities activitiesR ->
                    case activitiesR of
                        Ok ( revision, activities ) ->
                            Loading dateM
                                (Just (Store.init token revision activities))
                                token
                                |> updateLoading

                        Err err ->
                            ( Error (Api.errorString err), Effect.None )

                _ ->
                    ( model, Effect.None )

        Error _ ->
            ( model, Effect.None )

        Loaded state ->
            let
                (State calendar store activityM) =
                    state
            in
            case msg of
                GotActivities _ ->
                    updateStore msg state |> loaded

                VisibilityChange visibility ->
                    case visibility of
                        Events.Visible ->
                            ( model, Effect.Batch [ Effect.DateToday LoadToday, Effect.GetActivities ] )

                        Events.Hidden ->
                            ( model, Store.flush store )

                KeyPressed key ->
                    case key of
                        "Enter" ->
                            updateActivityForm ClickedSubmit state
                                |> loaded

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
                    ( Loaded (State calendar store newActivityM)
                    , Effect.None
                    )

                AutoScrollCalendar y ->
                    let
                        distance =
                            50

                        navbarHeight =
                            50

                        autoScrollCalendar =
                            Dom.getViewportOf "calendar"
                                |> Task.andThen
                                    (\info ->
                                        if y < 0 then
                                            Task.succeed ()

                                        else if y > (info.viewport.height * 0.9 + navbarHeight) then
                                            Dom.setViewportOf "calendar" 0 (info.viewport.y + distance)

                                        else if y < (info.viewport.height * 0.1 + navbarHeight) then
                                            Dom.setViewportOf "calendar" 0 (info.viewport.y - distance)

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
                    ( Loaded (State calendar store newActivityM), Effect.None )

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
                                updateStore (Move date activity) (State calendar store newActivityM) |> loaded

                        _ ->
                            ( model, Effect.None )

                NoOp ->
                    ( model, Effect.None )

                Create activity ->
                    updateStore msg (State calendar store (Selected [ activity ])) |> loaded

                Group activities session ->
                    updateStore msg (State calendar store (Selected [ session ])) |> loaded

                Ungroup activities session ->
                    updateStore msg (State calendar store (Selected activities)) |> loaded

                Update activity ->
                    updateStore msg (State calendar store (Selected [ activity ])) |> loaded

                Move _ _ ->
                    updateStore msg state |> loaded

                Shift _ _ ->
                    case activityM of
                        Editing form ->
                            updateActivityForm msg state
                                |> loaded

                        _ ->
                            updateStore msg state |> loaded

                Delete _ ->
                    case activityM of
                        Editing form ->
                            if List.length (Tuple.second form.laps) == 1 then
                                updateStore msg (State calendar store None) |> loaded

                            else
                                updateActivityForm msg state
                                    |> loaded

                        _ ->
                            updateStore msg (State calendar store None) |> loaded

                Posted _ _ ->
                    updateStore msg state |> loaded

                DebounceFlush _ ->
                    updateStore msg state |> loaded

                LoadToday date ->
                    updateCalendar msg state
                        |> loaded

                Jump _ ->
                    updateCalendar msg state
                        |> loaded

                ChangeZoom zoom dateM ->
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

                ScrollCompleted _ ->
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
                    updateStore (Create activity) (State calendar store (Editing form))
                        |> loaded

                EditActivity activity ->
                    let
                        form =
                            ActivityForm.init activity
                    in
                    ( Loaded <| State calendar store (Editing form), Effect.None )

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
                            ( Loaded <| State calendar store (Selected list), Effect.None )

                        _ ->
                            ( Loaded <| State calendar store (Selected [ activity ]), Effect.None )

                MoveActivity activity ->
                    ( Loaded <| State calendar store (Moving activity -100 -100), Effect.None )

                SelectedDate _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedDescription _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedLap _ ->
                    updateActivityForm msg state
                        |> loaded

                ClickedAddLap ->
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

                SelectedActivityType _ ->
                    updateActivityForm msg state
                        |> loaded

                EditedDuration _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedPace _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedRace _ ->
                    updateActivityForm msg state
                        |> loaded

                SelectedEffort _ ->
                    updateActivityForm msg state
                        |> loaded

                ClickedSubmit ->
                    case activityM of
                        Editing form ->
                            updateActivityForm msg state
                                |> loaded

                        _ ->
                            ( Loaded (State calendar store None), Effect.None )

                ClickedCopy activity ->
                    case activityM of
                        Editing form ->
                            updateActivityForm msg state
                                |> loaded

                        _ ->
                            ( model
                            , Activity.newId
                                |> Random.map (\id -> { activity | id = id })
                                |> Random.generate NewActivity
                                |> Effect.Cmd
                            )

                ClickedMove ->
                    let
                        ( calendarState, calendarCmd ) =
                            updateCalendar (ChangeZoom Msg.Year Nothing) state

                        ( activityFormState, activityFormCmd ) =
                            updateActivityForm msg calendarState
                    in
                    ( activityFormState, Effect.Batch [ calendarCmd, activityFormCmd ] )
                        |> loaded

                ClickedUngroup session ->
                    case session.laps of
                        Just dataList ->
                            ( model
                            , Random.list (List.length dataList) Activity.newId
                                |> Random.map
                                    (\ids ->
                                        List.map2
                                            (\id data -> Activity id session.date "" data Nothing)
                                            ids
                                            dataList
                                    )
                                |> Random.generate (\activities -> Ungroup activities session)
                                |> Effect.Cmd
                            )

                        _ ->
                            ( model, Effect.None )

                ClickedGroup ->
                    case activityM of
                        Selected (a :: tail) ->
                            ( model, initSession a (a :: tail) )

                        _ ->
                            ( model, Effect.None )

                ClickedClose ->
                    ( Loaded (State calendar store None), Effect.None )

                NewId _ ->
                    updateActivityForm msg state
                        |> loaded


updateLoading : Model -> ( Model, Effect )
updateLoading model =
    case model of
        Loading (Just date) (Just store) csrfToken ->
            (Loaded <|
                State
                    (Calendar.init Msg.Month date date)
                    store
                    None
            )
                |> update (Jump date)

        _ ->
            ( model, Effect.None )


updateActivityForm : Msg -> State -> ( State, Effect )
updateActivityForm msg (State calendar store activityM) =
    let
        ( newActivityM, cmd ) =
            case activityM of
                Editing form ->
                    ActivityForm.update msg form |> Tuple.mapFirst Editing

                _ ->
                    ( activityM, Effect.None )
    in
    ( State calendar store newActivityM, cmd )


updateCalendar : Msg -> State -> ( State, Effect )
updateCalendar msg (State calendar store activityM) =
    Calendar.update msg calendar
        |> Tuple.mapFirst (\updated -> State updated store activityM)


updateStore : Msg -> State -> ( State, Effect )
updateStore msg (State calendar store activityM) =
    Store.update msg store
        |> Tuple.mapFirst (\updated -> State calendar updated activityM)


loaded : ( State, Effect ) -> ( Model, Effect )
loaded stateTuple =
    Tuple.mapFirst Loaded stateTuple


initActivity : Date -> Maybe Date -> Effect
initActivity today dateM =
    let
        date =
            dateM |> Maybe.withDefault today

        completed =
            if Date.compare date today == LT || date == today then
                Activity.Completed

            else
                Activity.Planned

        activityData =
            Activity.initActivityData
    in
    Activity.newId
        |> Random.map
            (\id ->
                Activity id
                    date
                    ""
                    { activityData | completed = completed }
                    Nothing
            )
        |> Effect.GenerateActivity NewActivity


initSession : Activity -> List Activity -> Effect
initSession head activities =
    let
        activityData =
            Activity.initActivityData
    in
    Activity.newId
        |> Random.map
            (\id ->
                Activity id
                    head.date
                    ""
                    { activityData | completed = head.data.completed }
                    (Just (List.map .data activities))
            )
        |> Effect.GenerateActivity (Group activities)



-- VIEW


view : Model -> Html Msg
view model =
    expandingRow
        [ id "home"
        , borderStyle "border-left"
        , borderStyle "border-right"
        ]
        [ case model of
            Loading _ _ _ ->
                column [] [ text "Loading" ]

            Error errorString ->
                column [] [ text errorString ]

            Loaded (State calendar store activityM) ->
                let
                    activities =
                        Store.get store .activities

                    levelM =
                        Store.get store .level

                    events =
                        case activityM of
                            Moving _ _ _ ->
                                [ Html.Events.on "pointermove" mouseMoveDecoder
                                , Html.Events.on "pointerup" (Decode.succeed MouseReleased)
                                , style "touch-action" "none"
                                , style "pointer-action" "none"
                                ]

                            _ ->
                                []

                    activeId =
                        case activityM of
                            Selected list ->
                                List.map .id list |> String.join " "

                            Editing { activity } ->
                                activity.id

                            Moving { id } _ _ ->
                                id

                            None ->
                                ""

                    activeRataDie =
                        case activityM of
                            Editing { date } ->
                                Maybe.map Date.toRataDie date |> Maybe.withDefault 0

                            Selected (a :: _) ->
                                Date.toRataDie a.date

                            _ ->
                                0
                in
                column (style "position" "relative" :: events)
                    [ Html.Lazy.lazy Calendar.viewHeader calendar
                    , Html.Lazy.lazy5 Calendar.view calendar activities activeId activeRataDie levelM
                    , Html.Lazy.lazy2 viewActivityM levelM activityM
                    , Html.Lazy.lazy2 ActivityForm.view levelM activityM
                    ]
        ]


viewActivityM : Maybe Int -> ActivityState -> Html Msg
viewActivityM levelM activityState =
    case activityState of
        Moving activity x y ->
            row
                [ style "position" "fixed"
                , style "left" (String.fromFloat x ++ "px")
                , style "top" (String.fromFloat y ++ "px")
                , style "z-index" "3"
                ]
                [ compactColumn [ style "flex-basis" "5rem" ]
                    [ ActivityShape.view levelM activity.data ]
                ]

        _ ->
            Html.text ""


mouseMoveDecoder =
    Decode.map2 MouseMoved
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)



-- SUBSCRIPTIONS


keyPressDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded (State _ _ activityM) ->
            Sub.batch
                [ Ports.selectDateFromScroll ReceiveSelectDate
                , Events.onVisibilityChange VisibilityChange
                , case activityM of
                    Editing form ->
                        Events.onKeyPress keyPressDecoder

                    Selected _ ->
                        Events.onKeyPress keyPressDecoder

                    Moving activity x y ->
                        Time.every 100 (\_ -> AutoScrollCalendar y)

                    _ ->
                        Sub.none
                ]

        _ ->
            Sub.none
