module Calendar exposing (Model, get, init, update, view, viewHeader, viewMenu)

import Activity exposing (Activity)
import ActivityForm
import ActivityShape
import Browser.Dom as Dom
import Date exposing (Date)
import Duration
import Html exposing (Html, a, button, div, i, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseOver)
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import MonoIcons
import Msg exposing (ActivityState(..), Msg(..), Zoom(..))
import Pace
import Ports exposing (scrollToSelectedDate)
import Process
import Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, row, spinner, styleIf, viewIf, viewMaybe)
import Task
import Time exposing (Month(..))


type
    Model
    -- zoom start end selected today scrollCompleted
    = Model Zoom Date Date Date Date Bool


get : Model -> { zoom : Zoom, start : Date, end : Date, selected : Date, today : Date, scrollCompleted : Bool }
get (Model zoom start end selected today scrollCompleted) =
    { zoom = zoom, start = start, end = end, selected = selected, today = today, scrollCompleted = scrollCompleted }


init : Zoom -> Date -> Date -> Model
init zoom selected today =
    Model zoom (Date.add Date.Months -3 selected) (Date.add Date.Months 3 selected) selected today True


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    case msg of
        LoadToday date ->
            ( Model zoom start end selected date scrollCompleted, Cmd.none )

        Jump date ->
            ( init zoom date today, scrollToSelectedDate () )

        ChangeZoom newZoom dateM ->
            ( init newZoom (Maybe.withDefault selected dateM) today
            , scrollToSelectedDate ()
            )

        Scroll up date currentHeight ->
            if not scrollCompleted then
                ( model, Cmd.none )

            else if up then
                ( Model zoom date end selected today False
                , returnScroll currentHeight
                )

            else
                ( Model zoom start date selected today scrollCompleted
                , Cmd.none
                )

        ScrollCompleted result ->
            ( Model zoom start end selected today True
            , Cmd.none
            )

        ReceiveSelectDate selectDate ->
            let
                newSelected =
                    Date.fromIsoString selectDate |> Result.withDefault selected
            in
            if newSelected == selected then
                ( model, Cmd.none )

            else
                ( Model zoom start end newSelected today scrollCompleted, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW MENU


viewMenu : Model -> Html Msg
viewMenu model =
    let
        (Model zoom start end selected today scrollCompleted) =
            model
    in
    row []
        [ compactColumn [ style "justify-content" "center" ] [ viewBackButton model ]
        , column []
            [ row [ style "justify-content" "center" ]
                [ viewDatePicker model
                , button
                    [ style "margin-left" "0.2rem"
                    , onClick (Jump today)
                    ]
                    [ text "Today" ]
                ]
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
            div [ class "dropdown" ]
                [ button []
                    [ text (Date.format "yyyy" selected)
                    ]
                , div [ class "dropdown-content" ]
                    (listYears selected Jump)
                ]

        Month ->
            div [ class "dropdown" ]
                [ button []
                    [ text (Date.format "MMMM" selected)
                    ]
                , div [ class "dropdown-content" ]
                    (listMonths selected Jump)
                ]

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


view : Model -> List Activity -> String -> Int -> Maybe Int -> Html Msg
view model activities activeId activeRataDie levelM =
    let
        (Model zoom start end selected today scrollCompleted) =
            model

        dayRows date =
            List.concat
                [ [ ( Date.toIsoString date, Html.Lazy.lazy3 viewDay (date == today) (date == selected) (Date.toRataDie date) ) ]
                , filterActivities date activities
                    |> List.map
                        (\activity ->
                            ( activity.id
                            , Html.Lazy.lazy4 viewActivity
                                (String.contains activity.id activeId)
                                (Date.toRataDie date == activeRataDie)
                                levelM
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
                                , Html.Lazy.lazy6 viewWeek
                                    activities
                                    today
                                    selected
                                    d
                                    activeId
                                    levelM
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
            ]
          <|
            List.concat
                [ [ ( "loadingup", loadingSpinner ) ]
                , body
                , [ ( "loadingdown", loadingSpinner ) ]
                ]
        ]


viewActivityShape : Activity -> Bool -> Maybe Int -> Html Msg
viewActivityShape activity isActive levelM =
    div
        [ style "width" "min-content"
        , Html.Events.on "pointerdown" (Decode.succeed (MoveActivity activity))
        , attributeIf isActive (class "dynamic-shape")
        , style "touch-action" "none"
        ]
        [ ActivityShape.view levelM activity.data ]



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


returnScroll : Int -> Cmd Msg
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
                                column [ style "background" "white", style "color" "var(--icon-gray)" ]
                                    [ text d ]
                            )
                   )
            )


viewWeek : List Activity -> Date -> Date -> Date -> String -> Maybe Int -> Html Msg
viewWeek allActivities today selected start activeId levelM =
    let
        days =
            daysOfWeek start

        isNewMonth =
            days
                |> List.any (\d -> Date.day d == 1)

        dayViews =
            days
                |> List.map (\d -> viewWeekDay ( d, filterActivities d allActivities ) (d == today) (d == selected) activeId levelM)

        activities =
            days
                |> List.map (\d -> filterActivities d allActivities)
                |> List.concat
    in
    row [ style "padding" "0 0.5rem", styleIf isNewMonth "margin-top" "1rem" ] <|
        titleWeek activities
            :: dayViews


viewWeekDay : ( Date, List Activity ) -> Bool -> Bool -> String -> Maybe Int -> Html Msg
viewWeekDay ( date, activities ) isToday isSelected activeId levelM =
    let
        isActive a =
            activeId == a.id
    in
    column
        [ attributeIf isSelected (id "selected-date")
        , style "min-height" "4rem"
        , style "padding-bottom" "1rem"
        , Html.Events.on "pointerenter" (Decode.succeed (MoveTo date))
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
                    [ onClick (ChangeZoom Month (Just date))
                    , attribute "data-date" (Date.toIsoString date)
                    , styleIf isToday "text-decoration" "underline"
                    ]
                    [ text (Date.format "d" date) ]
                ]
            :: List.map
                (\a ->
                    row
                        [ attributeIf (not (isActive a)) (Html.Events.on "pointerdown" (pointerDownDecoder a))
                        , class "no-select"
                        , style "margin-bottom" "0.1rem"
                        , style "margin-right" "0.2rem"
                        , attributeIf (isActive a) (style "opacity" "0.5")
                        ]
                        [ viewActivityShape a (isActive a) levelM ]
                )
                activities


titleWeek : List Activity -> Html msg
titleWeek activities =
    let
        sumDuration datas =
            datas
                |> List.map
                    (\data ->
                        case data.pace of
                            Just pace ->
                                ( data.duration, 0 )

                            Nothing ->
                                ( 0, data.duration )
                    )
                |> List.foldl (\( r, o ) ( sr, so ) -> ( sr + r, so + o )) ( 0, 0 )

        ( runDuration, otherDuration ) =
            sumDuration (List.map .data activities)

        hours duration =
            (duration // 60) // 60

        minutes duration =
            remainderBy 60 (duration // 60)
    in
    column
        [ style "min-width" "4rem" ]
        [ row [ style "color" "var(--activity-green)" ]
            [ text <|
                if runDuration /= 0 then
                    List.foldr (++) "" [ String.fromInt (hours runDuration), "h ", String.fromInt (minutes runDuration), "m" ]

                else
                    ""
            ]
        , row [ style "color" "var(--activity-gray)" ]
            [ text <|
                if otherDuration /= 0 then
                    List.foldr (++) "" [ String.fromInt (hours otherDuration), "h ", String.fromInt (minutes otherDuration), "m" ]

                else
                    ""
            ]
        ]


weekList : Date -> Date -> List Date
weekList start end =
    Date.range Date.Week 1 (Date.floor Date.Week start) end


daysOfWeek : Date -> List Date
daysOfWeek start =
    Date.range Date.Day 1 start (Date.add Date.Weeks 1 start)



-- MONTH VIEW


viewDay : Bool -> Bool -> Int -> Html Msg
viewDay isToday isSelected rataDie =
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
        , Html.Events.on "pointerenter" (Decode.succeed (MoveTo date))
        ]
        [ text (Date.format "E MMM d" date) ]


viewActivity : Bool -> Bool -> Maybe Int -> Activity -> Html Msg
viewActivity isActive isActiveDate levelM activity =
    let
        activityLevel =
            Activity.mprLevel activity
                |> Maybe.map (\l -> "level " ++ String.fromInt l)
                |> Maybe.withDefault ""

        trainingPaceStr paceM =
            case ( paceM, levelM ) of
                ( Just pace, Just level ) ->
                    Pace.secondsToTrainingPace level pace
                        |> Pace.trainingPace.toString
                        |> String.toLower

                ( Just pace, Nothing ) ->
                    " at " ++ Pace.paceToString pace ++ " pace"

                _ ->
                    ""
    in
    row
        [ style "padding" "0.5rem 0.5rem"
        , styleIf isActive "background-color" "var(--highlight-gray)"
        ]
        [ compactColumn
            [ style "flex-basis" "5rem"
            , style "justify-content" "center"
            , attributeIf (not isActive) (Html.Events.on "pointerdown" (pointerDownDecoder activity))
            ]
            [ viewActivityShape activity isActive levelM ]
        , a
            [ class "column expand"
            , style "justify-content" "center"
            , attributeIf (not isActive) (Html.Events.on "pointerdown" (pointerDownDecoder activity))
            ]
            [ row [] [ text activity.description ]
            , row [ style "font-size" "0.8rem" ]
                [ column []
                    [ text <|
                        String.join " "
                            [ Duration.toStringWithUnits activity.data.duration
                            , trainingPaceStr activity.data.pace
                            ]
                    ]
                , compactColumn [ style "align-items" "flex-end" ] [ text activityLevel ]
                ]
            ]
        , compactColumn
            [ attributeIf (not isActive)
                (Html.Events.on "pointerdown" (Decode.succeed (SelectActivity activity True)))
            , style "justify-content" "center"
            ]
            [ row
                [ style "width" "0.4rem"
                , style "height" "0.4rem"
                , style "border-radius" "50%"
                , style "border" "2px solid transparent"
                , attributeIf (isActiveDate || isActive) (borderStyle "border")
                , attributeIf isActive (style "background-color" "var(--icon-gray)")
                ]
                []
            ]
        ]


pointerDownDecoder : Activity -> Decode.Decoder Msg
pointerDownDecoder activity =
    Decode.map
        (SelectActivity activity)
        (Decode.field "shiftKey" Decode.bool)


viewAddButton : Date -> Html Msg
viewAddButton date =
    row [ style "padding" "0.5rem 0.5rem" ]
        [ compactColumn []
            [ a
                [ onClick (ClickedNewActivity date)
                , class "button small"
                , style "font-size" "0.8rem"
                , style "padding" "0.05rem 0.25rem"
                ]
                [ MonoIcons.icon (MonoIcons.add "var(--icon-gray)") ]
            ]
        ]


listDays : Date -> Date -> List Date
listDays start end =
    Date.range Date.Day 1 start end
