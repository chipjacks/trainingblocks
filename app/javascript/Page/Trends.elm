module Page.Trends exposing (main)

import Activity
import Activity.Aggregate as Aggregate
import Activity.Laps
import Activity.Types exposing (Activity, ActivityData, RaceDistance)
import Api
import Browser
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
    column [ style "margin-left" "10px", style "margin-right" "10px" ]
        [ Html.h3 [] [ Html.text "Level" ]
        , viewLevelChart model
        , Html.h3 [] [ Html.text "Hours per week" ]
        , viewTimeChart model
        ]



-- TODO: Distinguish planned vs completed activities, or just add lots of filtering abilities


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
        [ CA.height 200
        , CA.width 600
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


viewTimeChart : Model -> Svg msg
viewTimeChart { activities, year } =
    let
        data =
            Date.range Date.Week
                1
                (Date.fromCalendarDate year Time.Jan 1)
                (Date.fromCalendarDate (year + 1) Time.Jan 1)
                |> List.map
                    (\date ->
                        List.filter (\a -> Date.isBetween date (Date.add Date.Days 6 date) a.date) activities
                            |> (\acts ->
                                    { run = Aggregate.duration [ Aggregate.run, Aggregate.completed ] acts
                                    , other = Aggregate.duration [ Aggregate.other, Aggregate.completed ] acts
                                    , time = dateToPosixTime date
                                    }
                               )
                    )

        toHours secs =
            (secs |> toFloat) / (60 * 60)
    in
    C.chart
        [ CA.height 200
        , CA.width 600
        , CA.margin { top = 10, bottom = 40, left = 40, right = 40 }
        ]
        [ C.xTicks [ CA.times Time.utc ]
        , C.yTicks [ CA.ints ]
        , C.xLabels [ CA.times Time.utc ]
        , C.yLabels [ CA.ints ]
        , C.xAxis []
        , C.yAxis []
        , C.series (.time >> toFloat)
            [ C.stacked
                [ C.interpolated (.other >> toHours) [ CA.color "var(--blue-300)" ] []
                , C.interpolated (.run >> toHours) [ CA.color "var(--blue-500)" ] []
                ]
            ]
            data
        ]


epochStartOffset : Int
epochStartOffset =
    719162


dateToPosixTime : Date.Date -> Int
dateToPosixTime date =
    (Date.toRataDie date - epochStartOffset) * (1000 * 60 * 60 * 24) - (1000 * 60 * 60 * 24)
