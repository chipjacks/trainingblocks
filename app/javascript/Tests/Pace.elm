module Tests.Pace exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import MPRLevel
import Pace exposing (..)
import Pace.List exposing (PaceList)
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
        , describe "standardPaceToSeconds" <|
            [ test "returns the correct max pace in seconds" <|
                \_ ->
                    Expect.equal (standardPaceToSeconds (paces 47) Easy) (Just (7 * 60 + 35))
            ]
        , describe "secondsToStandardPace" <|
            [ test "returns the correct training pace for brisk" <|
                \_ ->
                    Expect.equal (secondsToStandardPace (paces 44) (6 * 60 + 29)) (Just Brisk)
            , test "returns the correct training pace for fast" <|
                \_ ->
                    Expect.equal (secondsToStandardPace (paces 44) (5 * 60 + 9)) (Just Fast)
            , fuzz2 (Fuzz.intRange 1 60)
                (Fuzz.oneOf (List.map (Tuple.second >> Fuzz.constant) standardPace.list))
                "reverses standardPaceToSeconds"
              <|
                \level pace ->
                    Expect.equal (standardPaceToSeconds (paces level) pace |> Maybe.andThen (secondsToStandardPace (paces level))) (Just pace)
            ]
        ]


paces : Int -> PaceList StandardPace
paces level =
    standardPaces ( MPRLevel.Neutral, level )
