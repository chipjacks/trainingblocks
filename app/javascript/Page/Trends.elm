module Page.Trends exposing (main)

import Activity
import Activity.Aggregate as Aggregate
import Activity.Data as Data
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, Completion(..), DistanceUnits(..), Effort(..), RaceDistance)
import Api
import App
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Date exposing (Date)
import Distance
import Duration
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Http
import Json.Encode as Encode
import MonoIcons
import Report
import Svg exposing (Svg)
import Task
import Time
import UI
import UI.Button
import UI.Dropdown
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton
import UI.Toast
import UI.Util exposing (viewMaybe)


main =
    App.document
        { init = init
        , title = "Trends"
        , update = update
        , perform = identity
        , view = \model -> view model
        , subscriptions = \_ -> Sub.none
        }


init : App.Env -> ( Model, Cmd Msg )
init _ =
    ( Model Loading [] 2020 (Date.fromCalendarDate 2021 Time.Jul 22) []
    , Cmd.batch
        [ Task.attempt GotActivities Api.getActivities
        , Task.perform GotToday Date.today
        ]
    )


type alias Model =
    { status : Status
    , activities : List Activity
    , year : Int
    , today : Date
    , raceHovering : List RaceItem
    }


type Status
    = Loading
    | Error String
    | Okay


type alias RaceItem =
    CI.One { level : Int, time : Int, race : Maybe RaceDistance, duration : Maybe Int, title : String, completed : Completion } CI.Dot


type Msg
    = GotToday Date
    | GotActivities (Result Http.Error ( String, List Activity ))
    | SelectedYear Int
    | OnRaceHover (List RaceItem)
    | NoOp


update : App.Env -> Msg -> Model -> ( Model, Cmd Msg )
update env msg model =
    case msg of
        GotToday today ->
            ( { model | year = Date.year today, today = today }, Cmd.none )

        GotActivities result ->
            case result of
                Ok ( _, activities ) ->
                    ( { model | status = Okay, activities = activities }, Cmd.none )

                Err err ->
                    ( { model | status = Error (Api.userError err) }
                    , reportError env "GotActivities" (Api.developerError err)
                    )

        SelectedYear year ->
            ( { model | year = year }, Cmd.none )

        OnRaceHover raceHovering ->
            ( { model | raceHovering = raceHovering }, Cmd.none )

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
        |> Skeleton.withTitle "Trends"
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withItems [ navHeader, yearDropdown ]
                |> (if model.status == Loading then
                        Navbar.withRightItem (UI.spinner "2rem")

                    else
                        identity
                   )
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html Msg
viewBody model =
    let
        headerMargin =
            style "margin" "20px 0 5px 0"

        errorM =
            case model.status of
                Error s ->
                    Just s

                _ ->
                    Nothing
    in
    column [ style "margin-left" "10px", style "margin-right" "10px", style "margin-bottom" "60px" ]
        [ viewMaybe errorM (\e -> UI.Toast.top |> UI.Toast.view (Html.text e))
        , viewChartHeader "Time" "Hours per week."
        , viewTimeChart model
        , viewChartHeader "Distance" "Miles per week."
        , viewDistanceChart model
        , viewChartHeader "Effort" "Hours per week."
        , viewEffortChart model
        , viewChartHeader "Races" "Performance level (0 － 60) is calculated for standard race distances from 5k to Marathon."
        , viewLevelChart model
        ]


viewChartHeader : String -> String -> Html msg
viewChartHeader title subtitle =
    let
        headerMargin =
            style "margin" "20px 0 5px 0"
    in
    compactColumn [ style "margin-bottom" "20px", style "margin-left" "15px" ]
        [ Html.h3 [ headerMargin ] [ Html.text title ]
        , Html.text subtitle
        ]


viewLevelChart : Model -> Html Msg
viewLevelChart { activities, year, raceHovering } =
    let
        start =
            Date.fromCalendarDate year Time.Jan 1

        end =
            Date.fromCalendarDate (year + 1) Time.Jan 1

        points =
            List.filter (\a -> Date.isBetween start end a.date) activities
                |> List.filterMap
                    (\a ->
                        Activity.mprLevel a
                            |> Maybe.map
                                (\( l, race ) ->
                                    { time = a.date |> dateToPosixTime
                                    , level = l
                                    , title = a.description
                                    , race = race.race
                                    , duration = race.duration
                                    , completed = race.completed
                                    }
                                )
                    )
    in
    yearChart
        [ CA.range
            [ CA.lowest (start |> dateToPosixTime |> toFloat) CA.exactly
            , CA.highest (end |> dateToPosixTime |> toFloat) CA.exactly
            ]
        , CA.domain
            [ CA.lowest 0 CA.exactly
            , CA.highest 60 CA.exactly
            ]
        , CE.onMouseMove OnRaceHover (CE.getNearest CI.dots)
        , CE.onMouseLeave (OnRaceHover [])
        ]
        [ C.series (.time >> toFloat)
            [ C.scatter (.level >> toFloat) [ CA.color "var(--red-300)", CA.size 24 ]
                |> C.named "Completed"
                |> C.amongst raceHovering (\d -> [ CA.highlight 0.15 ])
            ]
            (points |> List.filter (\d -> d.completed == Completed))
        , C.series (.time >> toFloat)
            [ C.scatter (.level >> toFloat) [ CA.color "white", CA.size 24, CA.borderWidth 3, CA.border "var(--red-300)" ]
                |> C.named "Planned"
                |> C.amongst raceHovering (\d -> [ CA.highlight 0.15 ])
            ]
            (points |> List.filter (\d -> d.completed == Planned))
        , C.each raceHovering <|
            \p item ->
                let
                    data =
                        CI.getData item
                in
                [ C.tooltip item
                    []
                    []
                    [ compactColumn [ style "font-size" "0.8rem" ]
                        [ row []
                            [ Html.div
                                [ style "max-width" "10rem"
                                , style "text-overflow" "ellipsis"
                                , style "white-space" "nowrap"
                                , style "overflow-x" "hidden"
                                ]
                                [ text data.title ]
                            ]
                        , row []
                            [ viewMaybe data.race (\r -> text (Activity.raceDistance.toString r))
                            , text " － "
                            , viewMaybe data.duration (\d -> text (Duration.toStringWithUnits d))
                            ]
                        , row []
                            [ text "Level "
                            , text (String.fromInt data.level)
                            ]
                        ]
                    ]
                ]
        ]


viewEffortChart : Model -> Svg msg
viewEffortChart { activities, year } =
    let
        data =
            weeklyActivities year
                activities
                (\acts ->
                    { none = Aggregate.duration [ Data.effort Nothing, Data.completed ] acts
                    , easy = Aggregate.duration [ Data.effort (Just Easy), Data.completed ] acts
                    , moderate = Aggregate.duration [ Data.effort (Just Moderate), Data.completed ] acts
                    , hard = Aggregate.duration [ Data.effort (Just Hard), Data.completed ] acts
                    }
                )

        toHours secs =
            (secs |> toFloat) / (60 * 60)
    in
    yearChart []
        [ C.bars
            [ CA.x1 (.start >> dateToPosixTime >> toFloat)
            , CA.x2 (.end >> dateToPosixTime >> toFloat)
            ]
            [ C.stacked
                [ C.bar (.data >> .hard >> toHours) [ CA.color "var(--red-300)", CA.roundTop 0.3 ]
                    |> C.named "Hard"
                , C.bar (.data >> .moderate >> toHours) [ CA.color "var(--orange-300)" ]
                    |> C.named "Moderate"
                , C.bar (.data >> .easy >> toHours) [ CA.color "var(--yellow-300)" ]
                    |> C.named "Easy"
                , C.bar (.data >> .none >> toHours) [ CA.color "var(--grey-900)" ]
                    |> C.named "None"
                ]
            ]
            data
        ]


viewTimeChart : Model -> Svg msg
viewTimeChart { activities, year } =
    let
        data =
            weeklyActivities year
                activities
                (\acts ->
                    { run = Aggregate.duration [ Data.run, Data.completed ] acts
                    , other = Aggregate.duration [ Data.other, Data.completed ] acts
                    }
                )

        toHours secs =
            (secs |> toFloat) / (60 * 60)
    in
    yearChart []
        [ C.bars
            [ CA.x1 (.start >> dateToPosixTime >> toFloat)
            , CA.x2 (.end >> dateToPosixTime >> toFloat)
            ]
            [ C.stacked
                [ C.bar (.data >> .other >> toHours) [ CA.color "var(--blue-300)", CA.roundTop 5, CA.roundBottom 5 ]
                    |> C.named "Other"
                , C.bar (.data >> .run >> toHours) [ CA.color "var(--blue-500)", CA.roundTop 0.3 ]
                    |> C.named "Run"
                ]
            ]
            data
        ]


viewDistanceChart : Model -> Svg msg
viewDistanceChart { activities, year } =
    let
        data =
            weeklyActivities year
                activities
                (Aggregate.distance [ Data.run, Data.completed ])

        toMiles meters =
            Distance.fromMeters Miles meters
    in
    yearChart []
        [ C.bars
            [ CA.x1 (.start >> dateToPosixTime >> toFloat)
            , CA.x2 (.end >> dateToPosixTime >> toFloat)
            ]
            [ C.stacked
                [ C.bar (.data >> toMiles) [ CA.color "var(--blue-500)", CA.roundTop 0.3 ]
                    |> C.named "Run"
                ]
            ]
            data
        ]



-- HELPERS


yearChart attrs info =
    C.chart
        ([ CA.height 300
         , CA.width 900
         , CA.margin { top = 10, bottom = 40, left = 40, right = 40 }
         ]
            ++ attrs
        )
        ([ C.xTicks [ CA.times Time.utc, CA.amount 13 ]
         , C.yTicks [ CA.ints ]
         , C.xLabels [ CA.times Time.utc, CA.amount 13 ]
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
         ]
            ++ info
        )


type alias TimeIntervalData a =
    { start : Date
    , end : Date
    , data : a
    }


weeklyActivities : Int -> List Activity -> (List Activity -> a) -> List (TimeIntervalData a)
weeklyActivities year activities toData =
    listWeeks year
        |> List.map
            (\date ->
                let
                    start =
                        date

                    end =
                        Date.add Date.Days 6 date

                    filter a =
                        Date.isBetween start end a.date
                in
                { start = start
                , end = end
                , data = List.filter filter activities |> toData
                }
            )


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



-- ERROR REPORTING


reportError : App.Env -> String -> String -> Cmd Msg
reportError env msg errorMsg =
    Report.error env
        |> Report.withField "msg" (Encode.string msg)
        |> Report.send errorMsg
        |> Task.attempt (\_ -> NoOp)
