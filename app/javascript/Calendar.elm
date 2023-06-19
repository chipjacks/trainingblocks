module Calendar exposing (Model, getToday, init, update, view, viewBackButton, viewHeader, viewMenu)

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
import UI.Button as Button
import UI.Dropdown
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (attributeIf, stopPropagationOnClick, styleIf, viewIf)


type
    Model
    -- Using a record here would be easier, but leads to performance issues with Html.Lazy.
    --      zoom start end  target position today scrollCompleted
    = Model Zoom Date Date (Maybe Date) Date Date Bool


init : Zoom -> Date -> Date -> ( Model, Effect )
init zoom selected today =
    ( Model zoom (Date.add Date.Months -3 selected) (Date.add Date.Months 3 selected) (Just selected) selected today True
    , Effect.Cmd (Process.sleep 300 |> Task.perform (\_ -> ClearTarget))
    )


getToday : Model -> Date
getToday (Model _ _ _ _ _ today _) =
    today


update : Msg -> Model -> ( Model, Effect )
update msg model =
    let
        (Model zoom start end target position today scrollCompleted) =
            model
    in
    case msg of
        LoadToday date ->
            ( Model zoom start end target position date scrollCompleted, Effect.None )

        Jump date ->
            init zoom date today

        ClearTarget ->
            ( Model zoom start end Nothing position today scrollCompleted, Effect.None )

        ChangeZoom newZoom dateM ->
            init newZoom (Maybe.withDefault position dateM) today

        Scroll up ->
            if not scrollCompleted then
                ( model, Effect.None )

            else if up then
                ( Model zoom (Date.add Date.Months -2 start) end (Just start) position today False
                , Effect.Cmd (Process.sleep 300 |> Task.perform (\_ -> ScrollCompleted))
                )

            else
                ( Model zoom start (Date.add Date.Months 2 end) Nothing position today True
                , Effect.None
                )

        ScrollCompleted ->
            ( Model zoom start end Nothing position today True
            , Effect.None
            )

        ReceiveSelectDate selectDate ->
            let
                newPosition =
                    Date.fromIsoString selectDate |> Result.withDefault position
            in
            if not scrollCompleted || newPosition == position then
                ( model, Effect.None )

            else
                ( Model zoom start end target newPosition today scrollCompleted, Effect.None )

        _ ->
            ( model, Effect.None )



-- VIEW MENU


viewMenu : Model -> List (Html Msg)
viewMenu model =
    let
        (Model _ _ _ _ _ today _) =
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
        (Model zoom _ _ _ position _ _) =
            model
    in
    case zoom of
        Year ->
            text ""

        Month ->
            a [ class "button row", style "margin-right" "0.2rem", style "align-items" "bottom", onClick (ChangeZoom Year Nothing) ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , text (Date.format "yyyy" position)
                ]

        Day ->
            a [ class "button", style "margin-right" "0.2rem", style "align-items" "bottom", onClick (ChangeZoom Month Nothing) ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , text (Date.format "MMMM yyyy" position)
                ]


viewDatePicker : Model -> Html Msg
viewDatePicker model =
    let
        (Model zoom _ _ _ position today _) =
            model
    in
    case zoom of
        Year ->
            UI.Dropdown.default
                (button [ class "button" ] [ text (Date.format "yyyy" position) ])
                (listYears today Jump)
                |> UI.Dropdown.view

        Month ->
            UI.Dropdown.default
                (button [ class "button" ] [ text (Date.format "MMMM" position) ])
                (listMonths position Jump)
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
        (Model zoom start end target position today _) =
            model

        dayRows date =
            ( Date.toIsoString date, Html.Lazy.lazy4 viewDay (date == today) (Just date == target) isMoving (Date.toRataDie date) )
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
                                    target
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
                    dayRows start

        viewLoadingSpinner up =
            row
                [ style "z-index" "3"
                , style "height" "0"
                ]
                [ column
                    [ styleIf (not up) "background-image" "linear-gradient(rgba(255,255,255,0), rgba(255,255,255,1))"
                    , styleIf up "background-image" "linear-gradient(rgba(255,255,255,1), rgba(255,255,255,0))"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "height" "2rem"
                    , style "padding" "1rem"
                    , styleIf (not up) "position" "relative"
                    , styleIf (not up) "bottom" "4.5rem"
                    ]
                    [ spinner "2rem" ]
                ]
    in
    Html.Keyed.node "infinite-calendar"
        [ id "calendar"
        , class "column expand no-select"
        , style "height" "fit-content"
        , style "width" "100%"
        , styleIf (zoom == Year) "animation" "slidein-left 0.5s"
        , styleIf (zoom == Month) "animation" "slidein-right 0.5s 0.01ms"
        , styleIf (zoom == Month) "opacity" "0"
        , styleIf (zoom == Month) "animation-fill-mode" "forwards"
        , attributeIf (activeId /= "") (stopPropagationOnClick (Decode.succeed ClickedClose))
        ]
    <|
        ( "loadingup", viewLoadingSpinner True )
            :: body
            ++ [ ( "loadingdown", viewLoadingSpinner False ) ]


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
            :: (Activity.Data.list [ Activity.Data.visible ] activity
                    |> List.map (\a -> ActivityShape.view configs a)
               )



-- YEAR VIEW


viewHeader : Model -> Maybe (Html Msg)
viewHeader (Model zoom _ _ _ _ _ _) =
    if zoom == Year then
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


viewWeek : List Activity -> Date -> Maybe Date -> Date -> Bool -> String -> ActivityConfigs -> Html Msg
viewWeek activities today target start isMoving activeId configs =
    let
        days =
            daysOfWeek start

        isNewMonth =
            days
                |> List.any (\d -> Date.day d == 1)

        dayViews =
            days
                |> List.map (\d -> viewWeekDay ( d, filterActivities d d activities ) (d == today) (Just d == target) isMoving activeId configs)
    in
    row [ style "padding" "0 0.5rem", styleIf isNewMonth "margin-top" "1rem" ] <|
        titleWeek activities
            :: dayViews


viewWeekDay : ( Date, List Activity ) -> Bool -> Bool -> Bool -> String -> ActivityConfigs -> Html Msg
viewWeekDay ( date, activities ) isToday isTarget isMoving activeId configs =
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
            :: viewIf isTarget viewScrollTarget
            :: row []
                [ Button.default (Date.format "d" date) NoOp
                    |> Button.withAppearance Button.Tall Button.Regular Button.None
                    |> Button.withAttributes
                        [ class "button--basic"
                        , onClick (ChangeZoom Month (Just date))
                        , style "margin-left" "-0.8rem"
                        , if isToday then
                            style "color" "var(--black-900)"

                          else
                            style "color" "var(--black-100)"
                        , styleIf isToday "font-weight" "bold"
                        ]
                    |> Button.view
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
            Activity.Aggregate.duration [ Activity.Data.run, Activity.Data.visible ] activities

        other =
            Activity.Aggregate.duration [ Activity.Data.other, Activity.Data.visible ] activities

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
                , style "z-index" "1"
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
viewDay isToday isTarget isMoving rataDie =
    let
        date =
            Date.fromRataDie rataDie
    in
    row
        [ attributeIf (Date.day date == 1) (class "month-header")
        , attribute "data-date" (Date.toIsoString date)
        , style "padding" "1rem 0.5rem"
        , styleIf isToday "font-weight" "bold"
        , style "color" "var(--black-500)"

        -- , onClick (ChangeZoom Day (Just date))
        , attributeIf isMoving (Html.Events.on "pointerenter" (Decode.succeed (MoveTo date)))
        ]
        [ viewIf isTarget viewScrollTarget
        , text (Date.format "E MMM d" date)
        ]


viewScrollTarget : Html msg
viewScrollTarget =
    -- Shifted up so sticky navbar is cleared when element is scrolled into view.
    Html.node "scroll-target" [ style "position" "relative", style "bottom" "calc(var(--navbar-height) + 1rem)" ] []


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
        , subtitle = Activity.View.activityDescription configs (Activity.Data.list [ Activity.Data.visible ] activity |> Activity.Laps.sum)
        , importM = activity.importId
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
