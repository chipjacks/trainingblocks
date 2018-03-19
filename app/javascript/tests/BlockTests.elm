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
                    \_ -> am
                        |> scale 2 3
                        |> (\b -> (b.w, b.h))
                        |> Expect.equal (40, 3)
                , test "works on nested blocks" <|
                    \_ -> activities
                        |> Activity.groupByType
                        |> List.map (List.map (initModel << Activity))
                        |> List.map sum
                        |> List.map (scale 2 3)
                        |> List.map .h
                        |> Expect.equal [6, 9]
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
                , test "sets view to normal" <|
                    \_ -> activities
                        |> Activity.groupByType
                        |> List.map (List.map (initModel << Activity))
                        |> List.map sum
                        |> List.map .view
                        |> Expect.equal [Normal, Normal]
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
                        |> List.map .view
                        |> Expect.equal [Split, Normal]
                ]
            , describe "#stack"
                [ test "shifts blocks on top of each other" <|
                    \_ -> ams
                        |> decompose
                        |> stack
                        |> decompose
                        |> List.map .x
                        |> Expect.equal [0, 5, 10, 15]
                , test "puts split blocks closer together" <|
                    \_ -> am
                        |> split 10
                        |> stack
                        |> decompose
                        |> List.map .x
                        |> Expect.equal [0, 2]
                ]
            , describe "#normalize"
                [ test "scales all blocks between 1 and 100" <|
                    \_ -> ams
                        |> decompose
                        |> normalize
                        |> List.map .w
                        |> Expect.equal [67, 44, 100, 22]
                ]
            ]
