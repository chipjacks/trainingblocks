module Page.Trends exposing (main)

import Activity
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, RaceDistance)
import Api
import Browser
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import MonoIcons
import Svg exposing (Svg)
import Task
import Time
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Skeleton as Skeleton


main =
    Browser.document
        { init = \x -> init x
        , view = \model -> { title = "Trends | Rhino Log", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] 2021
    , Task.attempt GotActivities Api.getActivities
    )


type alias Model =
    { activities : List Activity
    , year : Int
    }


type Msg
    = GotActivities (Result Http.Error ( String, List Activity ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActivities result ->
            case result of
                Ok ( _, activities ) ->
                    ( { model | activities = activities }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )


view model =
    let
        backButton =
            Html.a [ class "button row", style "align-items" "bottom", Html.Attributes.href "/calendar" ]
                [ MonoIcons.icon (MonoIcons.chevronLeft "#3d3d3d")
                , Html.text "Back"
                ]

        navHeader =
            Html.div [ style "font-size" "1.3rem", style "margin-top" "0.2rem" ] [ Html.text "Trends" ]
    in
    Skeleton.default
        |> Skeleton.withNavbar
            (Navbar.default
                |> Navbar.withBackButton backButton
                |> Navbar.withItems [ navHeader ]
                |> Navbar.view
            )
        |> Skeleton.withBody (viewBody model)
        |> Skeleton.view


viewBody : Model -> Html msg
viewBody model =
    column []
        [ viewLevelChart model.activities
        , viewTimeChart model
        ]


viewLevelChart : List Activity -> Svg msg
viewLevelChart activities =
    let
        chartConfig : LineChart.Config { a | level : Int, date : Date } msg
        chartConfig =
            { y = Axis.default 400 "Level" (.level >> toFloat)
            , x = Axis.time Time.utc 900 "Date" (.date >> dateToPosixTime >> toFloat)
            , container = Container.responsive "line-chart-1"
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.none
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.custom (Dots.full 4)
            }

        points =
            List.filterMap
                (\a -> Activity.mprLevel a |> Maybe.map (\l -> { date = a.date, level = l }))
                activities
    in
    LineChart.viewCustom chartConfig [ LineChart.line Colors.blueLight Dots.circle "Level" points ]


viewTimeChart : Model -> Svg msg
viewTimeChart { activities, year } =
    let
        chartConfig : LineChart.Config ( Date, Int ) msg
        chartConfig =
            { y = Axis.default 400 "Hours" (Tuple.second >> toFloat >> (\s -> s / (60 * 60)))
            , x = Axis.time Time.utc 900 "Date" (Tuple.first >> dateToPosixTime >> toFloat)
            , container = Container.responsive "line-chart-2"
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.default
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.stacked 0.5
            , line = Line.default
            , dots = Dots.custom (Dots.full 4)
            }

        weekDurations =
            Date.range Date.Week
                1
                (Date.fromCalendarDate year Time.Jan 1)
                (Date.fromCalendarDate (year + 1) Time.Jan 1)
                |> List.map
                    (\date ->
                        List.filter (\a -> Date.isBetween date (Date.add Date.Days 6 date) a.date) activities
                            |> Activity.sumDuration
                            |> Tuple.pair date
                    )

        runDurations =
            List.map (\( date, { run } ) -> ( date, run )) weekDurations

        otherDurations =
            List.map (\( date, { other } ) -> ( date, other )) weekDurations
    in
    LineChart.viewCustom chartConfig
        [ LineChart.line Colors.goldLight Dots.circle "Other" otherDurations
        , LineChart.line Colors.blueLight Dots.square "Run" runDurations
        ]


epochStartOffset : Int
epochStartOffset =
    719162


dateToPosixTime : Date.Date -> Int
dateToPosixTime date =
    (Date.toRataDie date - epochStartOffset) * (1000 * 60 * 60 * 24) - (1000 * 60 * 60 * 24)
