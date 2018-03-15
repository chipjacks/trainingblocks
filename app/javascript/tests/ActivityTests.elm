module ActivityTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Date exposing (Month(..))
import Date.Extra as Date exposing (fromCalendarDate)
import Activity exposing (..)


suite : Test
suite =
    let
        date = Date.fromCalendarDate 2018 Jan
        activities = 
            [ Model Run (date 1) 1 30
            , Model Run (date 2) 3 20
            , Model Ride (date 2) 3 45
            , Model Run (date 3) 2 10
            ]
    in
        describe "Activity"
            [ describe "#aggregateByType"
                [ test "groups activities by type" <|
                    \_ -> aggregateByType activities
                        |> List.map .type_
                        |> Expect.equal [Run, Ride]
                , test "sums activity duration" <|
                    \_ -> aggregateByType activities
                        |> List.map .durationMinutes
                        |> Expect.equal [60, 45]
                , test "averages activity intensity" <|
                    \_ -> aggregateByType activities
                        |> List.map .intensity
                        |> Expect.equal [2, 3]
                , test "uses earliest date" <|
                    \_ -> aggregateByType activities
                        |> List.map .date
                        |> Expect.equal [date 1, date 2]
                ]
            ]