module BlockTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Block exposing (..)
import TestHelpers exposing (activity, activities, date)
import Activity exposing (..)


suite : Test
suite =
    let
        am = initModel (Activity activity)
        ams = activities |> List.map (initModel << Activity) |> initModel << Blocks
        bm = initModel (Blocks [am])
    in
        describe "Block"
            [ describe "#initModel"
                [ test "saves the activity" <|
                    \_ -> am
                        |> .data
                        |> Expect.equal (Activity activity)
                , test "saves nested blocks" <|
                    \_ -> bm
                        |> .data
                        |> Expect.equal (Blocks [am])
                ]
            , describe "#scale"
                [ test "scales width and height" <|
                    \_ -> ams
                        |> scale 2 2
                        |> (\b -> (b.w, b.h))
                        |> Expect.equal (210, 18)
                ]
            , describe "#sum"
                [ test "sums activity durations" <|
                    \_ -> activities
                        |> Activity.groupByType
                        |> List.map (List.map (initModel << Activity))
                        |> List.map sum
                        |> List.map .w
                        |> Expect.equal [60, 45]
                , test "averages activity intensities" <|
                    \_ -> activities
                        |> Activity.groupByType
                        |> List.map (List.map (initModel << Activity))
                        |> List.map sum
                        |> List.map .h
                        |> Expect.equal [2, 3]
                ]
            , describe "#split"
                [ test "splits blocks that are too wide" <|
                    \_ -> am
                        |> split 10
                        |> List.map .w
                        |> Expect.equal [10, 10]
                , test "sets split to true" <|
                    \_ -> am
                        |> split 10
                        |> List.map .split
                        |> Expect.equal [True, False]
                ]
            , describe "#stack"
                [ test "shifts blocks on top of each other" <|
                    \_ -> am
                        |> split 10
                        |> stack
                        |> decompose
                        |> List.map .x
                        |> Expect.equal [0, 5]
                ]
            ]
