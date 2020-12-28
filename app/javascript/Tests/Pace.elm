module Tests.Pace exposing (suite)

import Expect exposing (Expectation)
import Fuzz
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
            ]
        , describe "trainingPaceToSeconds" <|
            [ test "returns the correct max pace in seconds" <|
                \_ ->
                    Expect.equal (trainingPaceToSeconds 47 Easy) (7 * 60 + 35)
            ]
        , describe "secondsToTrainingPace" <|
            [ test "returns the correct training pace" <|
                \_ ->
                    Expect.equal (secondsToTrainingPace 47 (6 * 60 + 29)) Steady
            , fuzz2 (Fuzz.intRange 1 60)
                (Fuzz.oneOf (List.map (Tuple.second >> Fuzz.constant) trainingPace.list))
                "reverses trainingPaceToSeconds"
              <|
                \level pace -> Expect.equal (trainingPaceToSeconds level pace |> secondsToTrainingPace level) pace
            ]
        ]
