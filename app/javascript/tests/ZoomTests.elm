module ZoomTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Date exposing (Month(..))
import Date.Extra as Date exposing (Interval(..))
import Zoom exposing (initModel)

suite : Test
suite =
    let
        year = initModel Year (Date.fromCalendarDate 2018 Feb 15)
        month = initModel Month (Date.fromCalendarDate 2018 Feb 15)
        week = initModel Week (Date.fromCalendarDate 2018 Feb 19)
    in
        describe "Zoom"
            [ describe "#initModel"
                [ test "sets correct date range for a year" <|
                    \_ -> year
                        |> (\z -> (z.start, z.end))
                        |> Expect.equal (Date.fromCalendarDate 2017 Feb 1, Date.fromCalendarDate 2018 Mar 1)
                , test "sets correct date range for a month" <|
                    \_ -> month
                        |> (\z -> (z.start, z.end))
                        |> Expect.equal (Date.fromCalendarDate 2018 Jan 15, Date.fromCalendarDate 2018 Feb 19)
                , test "sets correct date range for a week" <|
                    \_ -> week
                        |> (\z -> (z.start, z.end))
                        |> Expect.equal (Date.fromCalendarDate 2018 Feb 12, Date.fromCalendarDate 2018 Feb 19)
                ]
            , describe "#range"
                [ test "provides a list of weeks in the month" <|
                    \_ -> initModel Month (Date.fromCalendarDate 2018 Jan 30)
                        |> Zoom.range
                        |> List.map (\z -> z.start)
                        |> Expect.equal ([1, 8,15,22,29] |> List.map (Date.fromCalendarDate 2018 Jan))
                ]
            ]