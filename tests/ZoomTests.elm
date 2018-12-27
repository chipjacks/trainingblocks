module ZoomTests exposing (suite)

import Date exposing (Interval(..), Unit(..))
import Time exposing (Month(..))
import Expect exposing (Expectation)
import Test exposing (..)
import Zoom exposing (initModel)


suite : Test
suite =
    let
        year =
            initModel Years (Date.fromCalendarDate 2018 Feb 15)

        month =
            initModel Months (Date.fromCalendarDate 2018 Feb 15)

        week =
            initModel Weeks (Date.fromCalendarDate 2018 Feb 19)
    in
    describe "Zoom"
        [ describe "#initModel"
            [ test "rounds year date range to past 4 quarters" <|
                \_ ->
                    year
                        |> (\z -> ( z.start, z.end ))
                        |> Expect.equal ( Date.fromCalendarDate 2017 Apr 1, Date.fromCalendarDate 2018 Apr 1 )
            , test "rounds month date range to past 5 weeks" <|
                \_ ->
                    month
                        |> (\z -> ( z.start, z.end ))
                        |> Expect.equal ( Date.fromCalendarDate 2018 Jan 15, Date.fromCalendarDate 2018 Feb 19 )
            , test "rounds week date range to past 7 days" <|
                \_ ->
                    week
                        |> (\z -> ( z.start, z.end ))
                        |> Expect.equal ( Date.fromCalendarDate 2018 Feb 12, Date.fromCalendarDate 2018 Feb 19 )
            ]
        , describe "#range"
            [ test "provides a list of weeks in the month" <|
                \_ ->
                    initModel Months (Date.fromCalendarDate 2018 Jan 30)
                        |> Zoom.range
                        |> List.map (\z -> z.start)
                        |> Expect.equal ([ 1, 8, 15, 22, 29 ] |> List.map (Date.fromCalendarDate 2018 Jan))
            , test "should always include 5 weeks" <|
                \_ ->
                    initModel Months (Date.fromRataDie 736450)
                        |> Zoom.range
                        |> List.length
                        |> Expect.equal 5
            ]
        , describe "#jump"
            [ test "jumps between years" <|
                \_ ->
                    initModel Years (Date.fromCalendarDate 2018 Apr 1)
                        |> Zoom.jump 1
                        |> .end
                        |> Expect.equal (Date.fromCalendarDate 2019 Jan 1)
            , test "jumps between months" <|
                \_ ->
                    initModel Months (Date.fromCalendarDate 2018 Apr 1)
                        |> Zoom.jump 1
                        |> .end
                        |> Expect.equal (Date.fromCalendarDate 2018 May 7)
            ]
        ]
