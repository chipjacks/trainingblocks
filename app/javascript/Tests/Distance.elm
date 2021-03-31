module Tests.Distance exposing (suite)

import Activity.Types exposing (DistanceUnits(..), RaceDistance(..))
import Distance exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Test exposing (..)


expectRealClose =
    Expect.within (Absolute 0.05)


suite : Test
suite =
    describe "The Distance module" <|
        [ describe "toMeters" <|
            [ test "converts from miles" <|
                \_ ->
                    expectRealClose (toMeters Miles 10) 16093.44
            , test "converts from kilometers" <|
                \_ ->
                    expectRealClose (toMeters Kilometers 10) 10000
            , test "converts from yards" <|
                \_ ->
                    expectRealClose (toMeters Yards 440) 402.336
            , fuzz (Fuzz.floatRange 1000 100000) "reverses fromMeters" <|
                \i ->
                    expectRealClose (fromMeters Miles i |> toMeters Miles) i
            , fuzz (Fuzz.floatRange 1000 100000) "reverses fromMeters with yards" <|
                \i ->
                    expectRealClose (fromMeters Yards i |> toMeters Yards) i
            ]
        , describe "fromMeters" <|
            [ test "converts to miles" <|
                \_ ->
                    expectRealClose (fromMeters Miles 16093) 10
            , test "converts to kilometers" <|
                \_ ->
                    expectRealClose (fromMeters Kilometers 10000) 10
            , test "converts to yards" <|
                \_ ->
                    expectRealClose (fromMeters Yards 402) 439.6
            , fuzz (Fuzz.floatRange 2 30) "reverses toMeters" <|
                \i ->
                    expectRealClose (toMeters Miles i |> fromMeters Miles) i
            , fuzz (Fuzz.floatRange 50 10000) "reverses toMeters with yards" <|
                \i ->
                    expectRealClose (toMeters Yards i |> fromMeters Yards) i
            ]
        , describe "toRaceDistance" <|
            [ test "matches 5 km" <|
                \_ ->
                    Expect.equal (toRaceDistance 5000) (Just FiveK)
            , test "matches 3.1 mi" <|
                \_ ->
                    Expect.equal (toRaceDistance (toMeters Miles 3.1)) (Just FiveK)
            , test "matches 10 mile" <|
                \_ ->
                    Expect.equal (toRaceDistance (toMeters Miles 10)) (Just TenMile)
            , test "matches half marathon" <|
                \_ ->
                    Expect.equal (toRaceDistance (toMeters Miles 13.1)) (Just HalfMarathon)
            ]
        ]
