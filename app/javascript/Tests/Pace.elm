module Tests.Pace exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import MPRLevel
import Pace exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Pace module" <|
        [ describe "paceToString" <|
            [ test "converts a pace in seconds to MM::SS" <|
                \_ ->
                    Expect.equal (paceToString 345) "5:45"
            , test "zero pads seconds" <|
                \_ -> Expect.equal (paceToString 425) "7:05"
            ]
        , describe "paceFromString" <|
            [ fuzz (Fuzz.intRange 180 1200) "reverses paceToString" <|
                \i ->
                    Expect.equal (paceToString i |> paceFromString) (Just i)
            , test "returns the correct pace in seconds" <|
                \_ ->
                    Expect.equal (paceFromString "6:40") (Just (6 * 60 + 40))
            , test "returns Nothing for empty string" <|
                \_ ->
                    Expect.equal (paceFromString "") Nothing
            , test "parses 0:SS correctly" <|
                \_ ->
                    Expect.equal (paceFromString "0:30") (Just 30)
            ]
        , describe "trainingPaceToSeconds" <|
            [ test "returns the correct max pace in seconds" <|
                \_ ->
                    Expect.equal (trainingPaceToSeconds (paces 47) Easy) (7 * 60 + 35)
            ]
        , describe "secondsToTrainingPace" <|
            [ test "returns the correct training pace for brisk" <|
                \_ ->
                    Expect.equal (secondsToTrainingPace (paces 44) (6 * 60 + 29)) Brisk
            , test "returns the correct training pace for fast" <|
                \_ ->
                    Expect.equal (secondsToTrainingPace (paces 44) (5 * 60 + 9)) Fast
            , fuzz2 (Fuzz.intRange 1 60)
                (Fuzz.oneOf (List.map (Tuple.second >> Fuzz.constant) trainingPace.list))
                "reverses trainingPaceToSeconds"
              <|
                \level pace ->
                    Expect.equal (trainingPaceToSeconds (paces level) pace |> secondsToTrainingPace (paces level)) pace
            ]
        ]


paces level =
    trainingPaces ( MPRLevel.Neutral, level )
        |> Maybe.withDefault []
