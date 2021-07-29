module Page.Trends exposing (main)

import Activity
import Activity.Aggregate as Aggregate
import Activity.Data as Data
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, Effort(..), RaceDistance)
import Api
import App
import Chart as C
import Chart.Attributes as CA
import Chart.Item as CI
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import MonoIcons
import Svg exposing (Svg)
import Task
import Time
import UI
import UI.Button
import UI.Dropdown
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton


main =
    App.document
        { init = init
        , title = "Trends"
        , update = update
        , perform = identity
        , view = \model -> view model
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model [] 2020 (Date.fromCalendarDate 2021 Time.Jul 22)
    , Cmd.batch
        [ Task.attempt GotActivities Api.getActivities
        , Task.perform GotToday Date.today
        ]
    )


type alias Model =
    { activities : List Activity
    , year : Int
    , today : Date
    }


type Msg
    = GotToday Date
    | GotActivities (Result Http.Error ( String, List Activity ))
    | SelectedYear Int
    | NoOp


update : App.Env -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        GotToday today ->
            ( { model | year = Date.year today, today = today }, Cmd.none )

        GotActivities result ->
            case result of
                Ok ( _, activities ) ->
                    ( { model | activities = activities }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SelectedYear year ->
            ( { model | year = year }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view model =
    let
        navHeader =
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Trends" ]

        yearDropdown =
            UI.Dropdown.default
                (UI.Button.default (String.fromInt model.year) NoOp |> UI.Button.view)
                (List.range (Date.year model.today - 5) (Date.year model.today)
                    |> List.reverse
                    |> List.map (\y -> ( String.fromInt y, SelectedYear y ))
                )
                |> UI.Dropdown.withAttributes [ style "margin-left" "10px" ]
                |> UI.Dropdown.view
    in
    Skeleton.default
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withItems [ navHeader, yearDropdown ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html msg
viewBody model =
    column [ style "margin-left" "10px", style "margin-right" "10px" ]
        [ Html.h3 [] [ Html.text "Hours" ]
        , viewTimeChart model
        , Html.h3 [] [ Html.text "Effort" ]
        , viewEffortChart model
        , Html.h3 [] [ Html.text "Race level" ]
        , viewLevelChart model
        ]


viewLevelChart : Model -> Svg msg
viewLevelChart { activities, year } =
    let
        start =
            Date.fromCalendarDate year Time.Jan 1 |> dateToPosixTime |> toFloat

        end =
            Date.fromCalendarDate (year + 1) Time.Jan 1 |> dateToPosixTime |> toFloat

        points =
            List.filterMap
                (\a -> Activity.mprLevel a |> Maybe.map (\l -> { time = a.date |> dateToPosixTime, level = l }))
                activities
    in
    C.chart
        [ CA.height 300
        , CA.width 900
        , CA.margin { top = 10, bottom = 40, left = 40, right = 40 }
        , CA.range
            [ CA.lowest start CA.exactly
            , CA.highest end CA.exactly
            ]
        , CA.domain
            [ CA.lowest 0 CA.orHigher
            ]
        ]
        [ C.xTicks [ CA.times Time.utc ]
        , C.yTicks [ CA.ints ]
        , C.xLabels [ CA.times Time.utc ]
        , C.yLabels [ CA.ints ]
        , C.xAxis []
        , C.yAxis []
        , C.series (.time >> toFloat)
            [ C.scatter (.level >> toFloat) [ CA.color "var(--blue-500)" ]
            ]
            points
        ]


viewEffortChart : Model -> Svg msg
viewEffortChart { activities, year } =
    let
        data =
            listWeeks year
                |> List.map
                    (\date ->
                        List.filter (\a -> Date.isBetween date (Date.add Date.Days 6 date) a.date) activities
                            |> (\acts ->
                                    { none = Aggregate.duration [ Data.effort Nothing, Data.completed ] acts
                                    , easy = Aggregate.duration [ Data.effort (Just Easy), Data.completed ] acts
                                    , moderate = Aggregate.duration [ Data.effort (Just Moderate), Data.completed ] acts
                                    , hard = Aggregate.duration [ Data.effort (Just Hard), Data.completed ] acts
                                    , start = date
                                    , end = Date.add Date.Days 6 date
                                    }
                               )
                    )

        toHours secs =
            (secs |> toFloat) / (60 * 60)
    in
    C.chart
        [ CA.height 300
        , CA.width 900
        , CA.margin { top = 10, bottom = 40, left = 40, right = 40 }
        ]
        [ C.xTicks [ CA.times Time.utc, CA.amount 12 ]
        , C.yTicks [ CA.ints ]
        , C.xLabels [ CA.times Time.utc, CA.amount 12 ]
        , C.yLabels [ CA.ints ]
        , C.xAxis []
        , C.yAxis []
        , C.legendsAt .max
            .max
            [ CA.row
            , CA.moveUp 20
            , CA.alignRight
            , CA.spacing 15
            ]
            []
        , C.bars
            [ CA.x1 (.start >> dateToPosixTime >> toFloat)
            , CA.x2 (.end >> dateToPosixTime >> toFloat)
            ]
            [ C.stacked
                [ C.bar (.hard >> toHours) [ CA.color "var(--red-300)", CA.roundTop 0.3 ]
                    |> C.named "Hard"
                , C.bar (.moderate >> toHours) [ CA.color "var(--orange-300)" ]
                    |> C.named "Moderate"
                , C.bar (.easy >> toHours) [ CA.color "var(--yellow-300)" ]
                    |> C.named "Easy"
                , C.bar (.none >> toHours) [ CA.color "var(--grey-900)" ]
                    |> C.named "None"
                ]
            ]
            data
        ]


viewTimeChart : Model -> Svg msg
viewTimeChart { activities, year } =
    let
        data =
            listWeeks year
                |> List.map
                    (\date ->
                        List.filter (\a -> Date.isBetween date (Date.add Date.Days 6 date) a.date) activities
                            |> (\acts ->
                                    { run = Aggregate.duration [ Data.run, Data.completed ] acts
                                    , other = Aggregate.duration [ Data.other, Data.completed ] acts
                                    , start = date
                                    , end = Date.add Date.Days 6 date
                                    }
                               )
                    )

        toHours secs =
            (secs |> toFloat) / (60 * 60)
    in
    C.chart
        [ CA.height 300
        , CA.width 900
        , CA.margin { top = 10, bottom = 40, left = 40, right = 40 }
        ]
        [ C.xTicks [ CA.times Time.utc, CA.amount 12 ]
        , C.yTicks [ CA.ints ]
        , C.xLabels [ CA.times Time.utc, CA.amount 12 ]
        , C.yLabels [ CA.ints ]
        , C.xAxis []
        , C.yAxis []
        , C.legendsAt .max
            .max
            [ CA.row
            , CA.moveUp 20
            , CA.alignRight
            , CA.spacing 15
            ]
            []
        , C.bars
            [ CA.x1 (.start >> dateToPosixTime >> toFloat)
            , CA.x2 (.end >> dateToPosixTime >> toFloat)
            ]
            [ C.stacked
                [ C.bar (.other >> toHours) [ CA.color "var(--blue-300)", CA.roundTop 5, CA.roundBottom 5 ]
                    |> C.named "Other"
                , C.bar (.run >> toHours) [ CA.color "var(--blue-500)", CA.roundTop 0.3 ]
                    |> C.named "Run"
                ]
            ]
            data
        ]


dateToPosixTime : Date.Date -> Int
dateToPosixTime date =
    let
        epochStartOffset =
            719162

        millisecondsInADay =
            1000 * 60 * 60 * 24
    in
    (Date.toRataDie date - epochStartOffset) * millisecondsInADay - millisecondsInADay


listWeeks : Int -> List Date
listWeeks year =
    Date.range Date.Week
        1
        (Date.fromCalendarDate year Time.Jan 1)
        (Date.fromCalendarDate (year + 1) Time.Jan 1)
