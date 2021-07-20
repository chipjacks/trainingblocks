module Tests.Aggregate exposing (suite)

import Activity.Aggregate as Aggregate
import Expect exposing (Expectation)
import Fuzz
import Test exposing (..)
import Tests.Fixture as Fixture


suite : Test
suite =
    describe "The Activity.Aggregate module" <|
        [ describe "duration" <|
            let
                runCompleted =
                    22426

                otherCompleted =
                    6900

                totalCompleted =
                    otherCompleted + runCompleted

                runPlanned =
                    15120

                otherPlanned =
                    0

                totalPlanned =
                    runPlanned + otherPlanned
            in
            [ test "sums the duration of completed runs" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.completed, Aggregate.run ] Fixture.activities) runCompleted
            , test "sums the duration of completed others" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.completed, Aggregate.other ] Fixture.activities) otherCompleted
            , test "sums the duration of completed activities" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.completed ] Fixture.activities) totalCompleted
            , test "sums the duration of planned runs" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.planned, Aggregate.run ] Fixture.activities) runPlanned
            , test "sums the duration of planned others" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.planned, Aggregate.other ] Fixture.activities) otherPlanned
            , test "sums the duration of planned activities" <|
                \_ ->
                    Expect.equal (Aggregate.duration [ Aggregate.planned ] Fixture.activities) totalPlanned
            ]
        ]
