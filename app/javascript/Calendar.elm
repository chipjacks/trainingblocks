module Calendar exposing (Model, get, handleScroll, init, update, view, viewBackButton, viewHeader, viewMenu)

import Actions exposing (viewActivityActions, viewMultiSelectActions, viewPopoverActions)
import Activity
import Activity.Aggregate
import Activity.Data
import Activity.Laps
import Activity.Types exposing (Activity)
import Activity.View
import ActivityShape
import Browser.Dom as Dom
import Date exposing (Date)
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import MonoIcons
import Msg exposing (ActivityConfigs, ActivityState(..), Msg(..), Zoom(..))
import Process
import Task
import Time exposing (Month(..))
import UI exposing (spinner)
import UI.Dropdown
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (attributeIf, stopPropagationOnClick, styleIf, viewIf)


type
    Model
    -- zoom start end selected today scrollCompleted
    = Model Zoom Date Date Date Date Bool


get : Model -> { zoom : Zoom, start : Date, end : Date, selected : Date, today : Date, scrollCompleted : Bool }
get (Model zoom start end selected today scrollCompleted) =
    { zoom = zoom, start = start, end = end, selected = selected, today = today, scrollCompleted = scrollCompleted }


init : Zoom -> Date -> Date -> ( Model, Effect )
init zoom selected today =
    ( Model zoom (Date.add Date.Months -3 selected) (Date.add Date.Months 3 selected) selected today False
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

        ScrollCompleted ->
            ( Model zoom start end selected today True
            , Effect.None
            )

        ReceiveSelectDate selectDate ->
            let
                newSelected =
                    Date.fromIsoString selectDate |> Result.withDefault selected
            in
            if not scrollCompleted || newSelected == selected then
                ( model, Effect.None )

            else
                ( Model zoom start end newSelected today scrollCompleted, Effect.None )

        _ ->
            ( model, Effect.None )



-- VIEW MENU


viewMenu : Model -> List (Html Msg)
viewMenu model =
    let
        (Model _ _ _ _ today _) =
            model
    in
    [ viewDatePicker model
    , button
        [ style "margin-left" "0.2rem"
        , class "button"
        , onClick (Jump today)
        ]
        [ text "Today" ]
    ]


viewBackButton : Model -> Html Msg
viewBackButton model =
    let
        (Model zoom _ _ selected _ _) =
            model
    in
    case zoom of
        Year ->
            text ""

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
        (Model zoom _ _ selected today _) =
            model
    in
    case zoom of
        Year ->
            UI.Dropdown.default
                (button [ class "button" ] [ text (Date.format "yyyy" selected) ])
                (listYears today Jump)
                |> UI.Dropdown.view

        Month ->
            UI.Dropdown.default
                (button [ class "button" ] [ text (Date.format "MMMM" selected) ])
                (listMonths selected Jump)
                |> UI.Dropdown.view

        Day ->
            text ""


listMonths : Date -> (Date -> Msg) -> List ( String, Msg )
listMonths date changeDate =
    let
        start =
            Date.fromCalendarDate (Date.year date) Jan 1

        end =
            Date.fromCalendarDate (Date.add Date.Years 1 date |> Date.year) Jan 1
    in
    Date.range Date.Month 1 start end
        |> List.map (viewDropdownItem changeDate "MMMM")


listYears : Date -> (Date -> Msg) -> List ( String, Msg )
listYears today changeDate =
    let
        start =
            Date.add Date.Years -3 today

        end =
            Date.add Date.Years 3 today
    in
    Date.range Date.Month 12 start end
        |> List.map (viewDropdownItem changeDate "yyyy")


viewDropdownItem : (Date -> Msg) -> String -> Date -> ( String, Msg )
viewDropdownItem changeDate formatDate date =
    ( Date.format formatDate date, changeDate date )



-- VIEW


filterActivities : Date -> Date -> List Activity -> List Activity
filterActivities start end activities =
    List.filter (\a -> Date.isBetween start end a.date) activities


view : Model -> List Activity -> String -> Int -> Bool -> ActivityConfigs -> Html Msg
view model activities activeId activeRataDie isMoving configs =
    let
        (Model zoom start end selected today _) =
            model

        dayRows date =
            ( Date.toIsoString date, Html.Lazy.lazy4 viewDay (date == today) (date == selected) isMoving (Date.toRataDie date) )
                :: (filterActivities date date activities
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
                   )
                ++ [ ( Date.toIsoString date ++ "+", Html.Lazy.lazy viewAddButton date ) ]

        body =
            case zoom of
                Year ->
                    weekList start end
                        |> List.map
                            (\d ->
                                ( Date.toIsoString d
                                , Html.Lazy.lazy7 viewWeek
                                    (filterActivities d (Date.add Date.Weeks 1 d) activities)
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
    Html.Keyed.node "div"
        [ id "calendar"
        , class "column expand no-select"
        , style "height" "fit-content"
        , styleIf (zoom == Year) "animation" "slidein-left 0.5s"
        , styleIf (zoom == Month) "animation" "slidein-right 0.5s 0.01ms"
        , styleIf (zoom == Month) "opacity" "0"
        , styleIf (zoom == Month) "animation-fill-mode" "forwards"
        , attributeIf (activeId /= "") (stopPropagationOnClick (Decode.succeed ClickedClose))
        ]
    <|
        (( "loadingup", loadingSpinner )
            :: body
            ++ [ ( "loadingdown", loadingSpinner ) ]
        )


viewActivityShape : Activity -> Bool -> Bool -> ActivityConfigs -> Html Msg
viewActivityShape activity isActive isMonthView configs =
    div
        [ style "width" "min-content"
        , attributeIf isActive (Html.Events.stopPropagationOn "pointerdown" (Decode.succeed ( MoveActivity activity, True )))
        , styleIf isActive "cursor" "grab"
        , attributeIf isActive (class "dynamic-shape")
        , style "touch-action" "none"
        , style "position" "relative"
        ]
    <|
        viewIf (isActive && isMonthView)
            (div
                [ style "position" "absolute"
                , style "top" "0"
                , style "left" "-55px"
                , style "width" "50px"
                , style "z-index" "3"
                ]
                [ viewPopoverActions ]
            )
            :: (Activity.Data.list [ Activity.Data.visible activity ] activity
                    |> List.map (\a -> ActivityShape.view configs a)
               )



-- SCROLLING


handleScroll : Model -> (Decode.Value -> Result Decode.Error Msg)
handleScroll (Model _ start end _ _ scrollCompleted) =
    let
        loadMargin =
            10

        ( loadPrevious, loadNext ) =
            ( Date.add Date.Months -2 start, Date.add Date.Months 2 end )
                |> Tuple.mapBoth (Scroll True) (Scroll False)
    in
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.at [ "target", "scrollingElement", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "scrollingElement", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollingElement", "clientHeight" ] Decode.int)
        |> Decode.andThen
            (\( scrollTop, scrollHeight, clientHeight ) ->
                if scrollTop < loadMargin then
                    Decode.succeed (loadPrevious scrollHeight)

                else if scrollTop > scrollHeight - clientHeight - loadMargin then
                    Decode.succeed (loadNext scrollHeight)

                else
                    Decode.fail ""
            )
        |> Decode.decodeValue


returnScroll : Int -> Effect
returnScroll previousHeight =
    Dom.getViewport
        |> Task.andThen
            (\info ->
                Task.sequence
                    [ Dom.setViewport 0 (info.scene.height - toFloat previousHeight)
                    , Process.sleep 100
                    , Dom.setViewport 0 (info.scene.height - toFloat previousHeight)
                    ]
            )
        |> Task.attempt (\_ -> ScrollCompleted)
        |> Effect.Cmd



-- YEAR VIEW


viewHeader : Model -> Maybe (Html Msg)
viewHeader model =
    if (model |> get |> .zoom) == Year then
        Just <|
            row []
                (column [ style "min-width" "4rem" ] []
                    :: ([ "M", "T", "W", "T", "F", "S", "S" ]
                            |> List.map
                                (\d ->
                                    column [ style "background" "white", style "color" "var(--grey-900)" ]
                                        [ text d ]
                                )
                       )
                )

    else
        Nothing


viewWeek : List Activity -> Date -> Date -> Date -> Bool -> String -> ActivityConfigs -> Html Msg
viewWeek activities today selected start isMoving activeId configs =
    let
        days =
            daysOfWeek start

        isNewMonth =
            days
                |> List.any (\d -> Date.day d == 1)

        dayViews =
            days
                |> List.map (\d -> viewWeekDay ( d, filterActivities d d activities ) (d == today) (d == selected) isMoving activeId configs)
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
        [ style "min-height" "4rem"
        , style "padding-bottom" "1rem"
        , attributeIf isMoving (Html.Events.on "pointerenter" (Decode.succeed (MoveTo date)))
        ]
    <|
        viewIf (Date.day date == 1)
            (row
                [ style "margin-top" "-1rem"
                , style "overflow-x" "visible"
                , style "max-width" "3rem"
                , style "white-space" "nowrap"
                , class "month-header"
                , attribute "data-date" (Date.toIsoString date)
                ]
                [ if Date.weekdayNumber date == 7 then
                    text (Date.format "MMM" date)

                  else
                    text (Date.format "MMMM" date)
                ]
            )
            :: viewSelectedDateScrollTarget isSelected
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
                        [ viewActivityShape a (isActive a) True configs
                        ]
                )
                activities


titleWeek : List Activity -> Html msg
titleWeek activities =
    let
        run =
            Activity.Aggregate.duration [ Activity.Data.run ] activities

        other =
            Activity.Aggregate.duration [ Activity.Data.other ] activities

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
        [ viewIf (run /= 0) (durationPillBox run "Run")
        , viewIf (other /= 0) (durationPillBox other "Other")
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
        , attribute "data-date" (Date.toIsoString date)
        , style "padding" "1rem 0.5rem"
        , styleIf isToday "font-weight" "bold"

        -- , onClick (ChangeZoom Day (Just date))
        , attributeIf isMoving (Html.Events.on "pointerenter" (Decode.succeed (MoveTo date)))
        ]
        [ viewSelectedDateScrollTarget isSelected
        , text (Date.format "E MMM d" date)
        ]


viewSelectedDateScrollTarget : Bool -> Html msg
viewSelectedDateScrollTarget isSelected =
    -- Shifted up so sticky navbar is cleared when element is scrolled into view.
    viewIf isSelected
        (row [ id "selected-date", style "position" "relative", style "bottom" "var(--navbar-height)" ] [])


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
        , subtitle = Activity.View.activityDescription Nothing (Activity.Data.list [ Activity.Data.visible activity ] activity |> Activity.Laps.sum)
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
            viewActivityShape activity isActive False configs
        }


selectActivityDecoder : Activity -> Decode.Decoder Msg
selectActivityDecoder activity =
    Decode.map
        (SelectActivity activity)
        (Decode.field "shiftKey" Decode.bool)


viewAddButton : Date -> Html Msg
viewAddButton date =
    Actions.viewAddAction (ClickedNewActivity date) "Add Activity"


listDays : Date -> Date -> List Date
listDays start end =
    Date.range Date.Day 1 start end
