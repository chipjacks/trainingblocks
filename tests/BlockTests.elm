module BlockTests exposing (suite)

import Block exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import TestHelpers exposing (activities, activity)


suite : Test
suite =
    let
        am =
            initModel (Activity (activity 1 3 20))

        ams =
            activities |> List.map (initModel << Activity) |> initModel << Blocks

        bm =
            initModel (Blocks [ am ])
    in
    describe "Block"
        [ describe "#initModel"
            [ test "saves the activity" <|
                \_ ->
                    am
                        |> .data
                        |> Expect.equal (Activity (activity 1 3 20))
            , test "saves nested blocks" <|
                \_ ->
                    bm
                        |> .data
                        |> Expect.equal (Blocks [ am ])
            ]
        , describe "#scale"
            [ test "scales width, height, x, and y" <|
                \_ ->
                    am
                        |> scale 2 3
                        |> (\b -> ( b.w, b.h, b.x, b.y ))
                        |> Expect.equal ( am.w * 2, am.h * 3, am.x * 2, am.y * 3 )
            , test "works on nested blocks" <|
                \_ ->
                    ams
                        |> decompose
                        |> List.indexedMap (\i b -> shift (i * 5) (i * 5) b)
                        |> sum
                        |> scale 2 3
                        |> decompose
                        |> List.map (\b -> ( b.w, b.h, b.x, b.y ))
                        |> Expect.equal
                            (ams
                                |> decompose
                                |> List.indexedMap (\i b -> shift (i * 5) (i * 5) b)
                                |> List.map (\b -> ( b.w * 2, b.h * 3, b.x * 2, b.y * 3 ))
                            )
            ]
        , describe "#sum"
            [ test "sums block widths" <|
                \_ ->
                    ams
                        |> decompose
                        |> sum
                        |> .w
                        |> Expect.equal (ams |> decompose |> List.map .w |> List.sum)
            , test "averages block heights" <|
                \_ ->
                    ams
                        |> decompose
                        |> sum
                        |> .h
                        |> Expect.equal
                            (ams
                                |> decompose
                                |> (\bs -> (bs |> List.map .h |> List.sum) // List.length bs)
                            )
            , test "sets view to normal" <|
                \_ ->
                    ams
                        |> decompose
                        |> sum
                        |> .view
                        |> Expect.equal Normal
            ]
        , describe "#split"
            [ test "splits blocks that are too wide" <|
                \_ ->
                    am
                        |> split 10
                        |> List.map .w
                        |> Expect.equal [ 10, 10 ]
            , test "sets split to true" <|
                \_ ->
                    am
                        |> split 10
                        |> List.map .view
                        |> Expect.equal [ Split, Normal ]
            ]
        , describe "#stack"
            [ test "shifts blocks on top of each other" <|
                \_ ->
                    ams
                        |> decompose
                        |> stack
                        |> decompose
                        |> List.map .x
                        |> Expect.equal [ 0, 5, 10 ]
            , test "puts split blocks closer together" <|
                \_ ->
                    am
                        |> split 10
                        |> stack
                        |> decompose
                        |> List.map .x
                        |> Expect.equal [ 0, 2 ]
            ]
        , describe "#normalize"
            [ test "scales all blocks between 1 and 100" <|
                \_ ->
                    ams
                        |> decompose
                        |> normalize
                        |> List.map .w
                        |> Expect.equal [ 67, 44, 100 ]
            ]
        ]
