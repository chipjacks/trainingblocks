module Calendar exposing (Model, get, init, update, view, viewHeader, viewMenu)

import Actions exposing (viewActivityActions, viewMultiSelectActions, viewPopoverActions)
import Activity
import Activity.Laps
import Activity.Types exposing (Activity)
import Activity.View
import ActivityShape
import Browser.Dom as Dom
import Date exposing (Date)
import Duration
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (on, onClick)
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import MonoIcons
import Msg exposing (ActivityConfigs, ActivityState(..), Msg(..), Zoom(..))
import Pace
import Ports exposing (scrollToSelectedDate)
import Process
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, dropdown, expandingRow, iconButton, row, spinner, stopPropagationOnClick, styleIf, viewIf, viewMaybe)
import Task
import Time exposing (Month(..))


type
    Model
    -- zoom start end selected today scrollCompleted
    = Model Zoom Date Date Date Date Bool


get : Model -> { zoom : Zoom, start : Date, end : Date, selected : Date, today : Date, scrollCompleted : Bool }
get (Model zoom start end selected today scrollCompleted) =
    { zoom = zoom, start = start, end = end, selected = selected, today = today, scrollCompleted = scrollCompleted }


init : Zoom -> Date -> Date -> ( Model, Effect )
init zoom selected today =
    ( Model zoom (Date.add Date.Months -3 selected) (Date.add Date.Months 3 selected) selected today True
    , Effect.ScrollToSelectedDate
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    case msg of
        LoadToday date ->
            ( Model zoom start end selected date scrollCompleted, Effect.None )

        Jump date ->
            init zoom date today

        ChangeZoom newZoom dateM ->
            init newZoom (Maybe.withDefault selected dateM) today

        Scroll up date currentHeight ->
            if not scrollCompleted then
                ( model, Effect.None )

            else if up then
                ( Model zoom date end selected today False
                , returnScroll currentHeight
                )

            else
                ( Model zoom start date selected today scrollCompleted
                , Effect.None
                )

        ScrollCompleted result ->
            ( Model zoom start end selected today True
            , Effect.None
            )

        ReceiveSelectDate selectDate ->
            let
                newSelected =
                    Date.fromIsoString selectDate |> Result.withDefault selected
            in
            if newSelected == selected then
                ( model, Effect.None )

            else
                ( Model zoom start end newSelected today scrollCompleted, Effect.None )

        _ ->
            ( model, Effect.None )



-- VIEW MENU


viewMenu : Bool -> Model -> Html Msg
viewMenu disabled model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    row []
        [ compactColumn [ style "justify-content" "center" ]
            [ if disabled then
                Skeleton.logo

              else
                viewBackButton model
            ]
        , column []
            [ viewIf (not disabled)
                (row [ style "justify-content" "center" ]
                    [ viewDatePicker model
                    , button
                        [ style "margin-left" "0.2rem"
                        , onClick (Jump today)
                        ]
                        [ text "Today" ]
                    ]
                )
            ]
        ]


viewBackButton : Model -> Html Msg
viewBackButton model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    case zoom of
        Year ->
            Skeleton.logo

        Month ->
            a [ class "button row", style "margin-right" "0.2rem", style "align-items" "bottom", onClick (ChangeZoom Year Nothing) ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , text (Date.format "yyyy" selected)
                ]

        Day ->
            a [ class "button", style "margin-right" "0.2rem", style "align-items" "bottom", onClick (ChangeZoom Month Nothing) ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , text (Date.format "MMMM yyyy" selected)
                ]


viewDatePicker : Model -> Html Msg
viewDatePicker model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    case zoom of
        Year ->
            dropdown False
                (button [] [ text (Date.format "yyyy" selected) ])
                (listYears selected Jump)

        Month ->
            dropdown False
                (button [] [ text (Date.format "MMMM" selected) ])
                (listMonths selected Jump)

        Day ->
            text ""


listMonths : Date -> (Date -> Msg) -> List (Html Msg)
listMonths date changeDate =
    let
        start =
            Date.fromCalendarDate (Date.year date) Jan 1

        end =
            Date.fromCalendarDate (Date.add Date.Years 1 date |> Date.year) Jan 1
    in
    Date.range Date.Month 1 start end
        |> List.map (viewDropdownItem changeDate "MMMM")


listYears : Date -> (Date -> Msg) -> List (Html Msg)
listYears date changeDate =
    let
        start =
            Date.add Date.Years -3 (Date.fromCalendarDate 2019 Time.Jan 1)

        end =
            Date.add Date.Years 3 (Date.fromCalendarDate 2019 Time.Jan 1)
    in
    Date.range Date.Month 12 start end
        |> List.map (viewDropdownItem changeDate "yyyy")


viewDropdownItem : (Date -> Msg) -> String -> Date -> Html Msg
viewDropdownItem changeDate formatDate date =
    a [ onClick (changeDate date) ] [ text <| Date.format formatDate date ]



-- VIEW


filterActivities : Date -> List Activity -> List Activity
filterActivities date activities =
    List.filter (\a -> a.date == date) activities


view : Model -> List Activity -> String -> Int -> Bool -> ActivityConfigs -> Html Msg
view model activities activeId activeRataDie isMoving configs =
    let
        (Model zoom start end selected today scrollCompleted) =
            model

        dayRows date =
            List.concat
                [ [ ( Date.toIsoString date, Html.Lazy.lazy4 viewDay (date == today) (date == selected) isMoving (Date.toRataDie date) ) ]
                , filterActivities date activities
                    |> List.map
                        (\activity ->
                            ( activity.id
                            , Html.Lazy.lazy4 viewActivity
                                activeId
                                (Date.toRataDie date == activeRataDie)
                                configs
                                activity
                            )
                        )
                , [ ( Date.toIsoString date ++ "+", Html.Lazy.lazy viewAddButton date ) ]
                ]

        body =
            case zoom of
                Year ->
                    weekList start end
                        |> List.map
                            (\d ->
                                ( Date.toIsoString d
                                , Html.Lazy.lazy7 viewWeek
                                    activities
                                    today
                                    selected
                                    d
                                    isMoving
                                    activeId
                                    configs
                                )
                            )

                Month ->
                    listDays start end
                        |> List.concatMap dayRows

                Day ->
                    dayRows selected

        loadingSpinner =
            viewIf (zoom /= Day) (row [ style "justify-content" "center", style "padding" "1rem" ] [ spinner "2rem" ])
    in
    expandingRow [ style "overflow" "hidden" ]
        [ Html.Keyed.node "div"
            [ id "calendar"
            , class "column expand"
            , style "overflow-y" "scroll"
            , style "overflow-x" "hidden"
            , attributeIf scrollCompleted (onScroll <| scrollHandler model)
            , class "no-select"
            , styleIf (zoom == Year) "animation" "slidein-left 0.5s"
            , styleIf (zoom == Month) "animation" "slidein-right 0.5s 0.01ms"
            , styleIf (zoom == Month) "opacity" "0"
            , styleIf (zoom == Month) "animation-fill-mode" "forwards"
            , attributeIf (activeId /= "") (Html.Events.stopPropagationOn "click" (Decode.succeed ( ClickedClose, True )))
            ]
          <|
            List.concat
                [ [ ( "loadingup", loadingSpinner ) ]
                , body
                , [ ( "loadingdown", loadingSpinner ) ]
                ]
        ]


viewActivityShape : Activity -> Bool -> ActivityConfigs -> Html Msg
viewActivityShape activity isActive configs =
    div
        [ style "width" "min-content"
        , attributeIf isActive (Html.Events.stopPropagationOn "pointerdown" (Decode.succeed ( MoveActivity activity, True )))
        , styleIf isActive "cursor" "grab"
        , attributeIf isActive (class "dynamic-shape")
        , style "touch-action" "none"
        , style "position" "relative"
        ]
    <|
        viewIf isActive
            (div
                [ style "position" "absolute"
                , style "top" "0"
                , style "left" "-55px"
                , style "width" "50px"
                , style "z-index" "3"
                ]
                [ viewPopoverActions ]
            )
            :: (Activity.Laps.listData activity
                    |> List.map (\a -> ActivityShape.view configs a)
               )



-- SCROLLING


onScroll : ( Int -> msg, Int -> msg ) -> Html.Attribute msg
onScroll ( loadPrevious, loadNext ) =
    let
        loadMargin =
            10
    in
    Html.Events.on "scroll"
        (Decode.map3 (\a b c -> ( a, b, c ))
            (Decode.at [ "target", "scrollTop" ] Decode.int)
            (Decode.at [ "target", "scrollHeight" ] Decode.int)
            (Decode.at [ "target", "clientHeight" ] Decode.int)
            |> Decode.andThen
                (\( scrollTop, scrollHeight, clientHeight ) ->
                    if scrollTop < loadMargin then
                        Decode.succeed (loadPrevious scrollHeight)

                    else if scrollTop > scrollHeight - clientHeight - loadMargin then
                        Decode.succeed (loadNext scrollHeight)

                    else
                        Decode.fail ""
                )
        )


returnScroll : Int -> Effect
returnScroll previousHeight =
    Dom.getViewportOf "calendar"
        |> Task.andThen
            (\info ->
                Task.sequence
                    [ Dom.setViewportOf "calendar" 0 (info.scene.height - toFloat previousHeight)
                    , Process.sleep 100
                    , Dom.setViewportOf "calendar" 0 (info.scene.height - toFloat previousHeight)
                    ]
            )
        |> Task.andThen (\_ -> Dom.getElement "calendar")
        |> Task.attempt (\result -> ScrollCompleted result)
        |> Effect.Cmd


scrollHandler : Model -> ( Int -> Msg, Int -> Msg )
scrollHandler (Model zoom start end _ _ _) =
    ( Date.add Date.Months -2 start, Date.add Date.Months 2 end )
        |> Tuple.mapBoth (Scroll True) (Scroll False)



-- YEAR VIEW


viewHeader : Model -> Html Msg
viewHeader model =
    viewIf ((model |> get |> .zoom) == Year) <|
        row [ borderStyle "border-bottom" ]
            (column [ style "min-width" "4rem" ] []
                :: ([ "M", "T", "W", "T", "F", "S", "S" ]
                        |> List.map
                            (\d ->
                                column [ style "background" "white", style "color" "var(--grey-900)" ]
                                    [ text d ]
                            )
                   )
            )


viewWeek : List Activity -> Date -> Date -> Date -> Bool -> String -> ActivityConfigs -> Html Msg
viewWeek allActivities today selected start isMoving activeId configs =
    let
        days =
            daysOfWeek start

        isNewMonth =
            days
                |> List.any (\d -> Date.day d == 1)

        dayViews =
            days
                |> List.map (\d -> viewWeekDay ( d, filterActivities d allActivities ) (d == today) (d == selected) isMoving activeId configs)

        activities =
            days
                |> List.map (\d -> filterActivities d allActivities)
                |> List.concat
    in
    row [ style "padding" "0 0.5rem", styleIf isNewMonth "margin-top" "1rem" ] <|
        titleWeek activities
            :: dayViews


viewWeekDay : ( Date, List Activity ) -> Bool -> Bool -> Bool -> String -> ActivityConfigs -> Html Msg
viewWeekDay ( date, activities ) isToday isSelected isMoving activeId configs =
    let
        isActive a =
            activeId == a.id
    in
    column
        [ attributeIf isSelected (id "selected-date")
        , style "min-height" "4rem"
        , style "padding-bottom" "1rem"
        , attributeIf isMoving (Html.Events.on "pointerenter" (Decode.succeed (MoveTo date)))
        ]
    <|
        viewIf (Date.day date == 1)
            (row [ style "margin-top" "-1rem", style "overflow-x" "visible", style "max-width" "3rem", style "white-space" "nowrap" ]
                [ if Date.weekdayNumber date == 7 then
                    text (Date.format "MMM" date)

                  else
                    text (Date.format "MMMM" date)
                ]
            )
            :: row []
                [ a
                    [ stopPropagationOnClick (Decode.succeed (ChangeZoom Month (Just date)))
                    , attribute "data-date" (Date.toIsoString date)
                    , styleIf isToday "text-decoration" "underline"
                    ]
                    [ text (Date.format "d" date) ]
                ]
            :: List.map
                (\a ->
                    row
                        [ attributeIf (not (isActive a))
                            (stopPropagationOnClick (selectActivityDecoder a))
                        , class "no-select"
                        , style "margin-bottom" "0.1rem"
                        , style "margin-right" "0.2rem"
                        , attributeIf (isActive a) (class "selected-shape")
                        , Html.Events.onDoubleClick (EditActivity a)
                        ]
                        [ viewActivityShape a (isActive a) configs
                        ]
                )
                activities


titleWeek : List Activity -> Html msg
titleWeek activities =
    let
        sumDuration datas =
            datas
                |> List.map
                    (\data ->
                        case data.activityType of
                            Activity.Types.Run ->
                                ( data.duration |> Maybe.withDefault 0, 0 )

                            _ ->
                                ( 0, data.duration |> Maybe.withDefault 0 )
                    )
                |> List.foldl (\( r, o ) ( sr, so ) -> ( sr + r, so + o )) ( 0, 0 )

        ( runDuration, otherDuration ) =
            sumDuration (List.map Activity.Laps.listData activities |> List.concat)

        hours duration =
            (duration // 60) // 60

        minutes duration =
            remainderBy 60 (duration // 60)

        durationPillBox seconds tag =
            row
                [ style "color" "var(--black-100)"
                , style "margin-bottom" "0.5rem"
                , style "margin-right" "0.2rem"
                , style "margin-left" "-3px"
                , style "flex-wrap" "wrap"
                ]
                [ compactColumn
                    [ style "background-color" "var(--grey-500)"
                    , styleIf (tag == "Other") "border-radius" "50%"
                    , styleIf (tag == "Run") "border-radius" "0.2rem"
                    , style "width" "1rem"
                    , style "height" "1rem"
                    , style "margin-top" "2px"
                    , style "margin-right" "3px"
                    , style "margin-bottom" "3px"
                    ]
                    []
                , div
                    [ style "background-color" "var(--grey-100)"
                    , style "padding-left" "0.3rem"
                    , style "padding-right" "0.3rem"
                    , style "border-radius" "0.7rem"
                    , style "white-space" "nowrap"
                    ]
                    [ text <|
                        List.foldr (++) "" [ String.fromInt (hours seconds), "h ", String.fromInt (minutes seconds), "m" ]
                    ]
                ]
    in
    column
        [ style "min-width" "4rem" ]
        [ viewIf (runDuration /= 0) (durationPillBox runDuration "Run")
        , viewIf (otherDuration /= 0) (durationPillBox otherDuration "Other")
        ]


weekList : Date -> Date -> List Date
weekList start end =
    Date.range Date.Week 1 (Date.floor Date.Week start) end


daysOfWeek : Date -> List Date
daysOfWeek start =
    Date.range Date.Day 1 start (Date.add Date.Weeks 1 start)



-- MONTH VIEW


viewDay : Bool -> Bool -> Bool -> Int -> Html Msg
viewDay isToday isSelected isMoving rataDie =
    let
        date =
            Date.fromRataDie rataDie
    in
    row
        [ attributeIf (Date.day date == 1) (class "month-header")
        , attributeIf isSelected (id "selected-date")
        , attribute "data-date" (Date.toIsoString date)
        , style "padding" "1rem 0.5rem"
        , styleIf isToday "font-weight" "bold"

        -- , onClick (ChangeZoom Day (Just date))
        , attributeIf isMoving (Html.Events.on "pointerenter" (Decode.succeed (MoveTo date)))
        ]
        [ text (Date.format "E MMM d" date) ]


viewActivity : String -> Bool -> ActivityConfigs -> Activity -> Html Msg
viewActivity activeIds isActiveDate configs activity =
    let
        isActive =
            String.contains activity.id activeIds

        isFirstActive =
            String.startsWith activity.id activeIds

        isMultiSelect =
            String.contains " " activeIds
    in
    Activity.View.listItem
        { titleM = Just activity.description
        , subtitle = Activity.View.activityDescription configs.levelM (Activity.Laps.visible activity |> Activity.Laps.sum)
        , isActive = isActive
        , handlePointerDown = selectActivityDecoder activity
        , handleDoubleClick = EditActivity activity
        , handleMultiSelectM =
            if isActive || isActiveDate then
                Just (Decode.succeed (SelectActivity activity True))

            else
                Nothing
        , viewToolbarM =
            if isMultiSelect && isFirstActive then
                Just viewMultiSelectActions

            else if isActive && not isMultiSelect then
                Just viewActivityActions

            else
                Nothing
        , viewShape =
            viewActivityShape activity isActive configs
        }


selectActivityDecoder : Activity -> Decode.Decoder Msg
selectActivityDecoder activity =
    Decode.map
        (SelectActivity activity)
        (Decode.field "shiftKey" Decode.bool)


viewAddButton : Date -> Html Msg
viewAddButton date =
    row [ Html.Attributes.class "add-button", style "margin-left" "0.5rem", style "margin-top" "0.5rem" ]
        [ iconButton
            [ stopPropagationOnClick (Decode.succeed (ClickedNewActivity date)), Html.Attributes.attribute "aria-label" "Add" ]
            [ MonoIcons.icon (MonoIcons.add "var(--grey-900)") ]
        ]


listDays : Date -> Date -> List Date
listDays start end =
    Date.range Date.Day 1 start end
