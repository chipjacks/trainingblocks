module Tests.Duration exposing (suite)

import Duration exposing (..)
import Expect exposing (Expectation)
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The Duration module" <|
        [ describe "timeStrToHrsMinsSecs" <|
            [ test "fails on seconds greater than 59" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "4:61" |> Result.toMaybe) Nothing
            , test "fails on seconds with three digits" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "4:031" |> Result.toMaybe) Nothing
            , test "succeeds for 00:MM:SS" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "00:04:31") (Ok [ 0, 4, 31 ])
            , test "succeeds for 0H:MM:SS" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "06:04:31") (Ok [ 6, 4, 31 ])
            , test "succeeds for HH:MM:SS" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "6:04:31") (Ok [ 6, 4, 31 ])
            , test "succeeds for M:SS" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "4:31") (Ok [ 4, 31 ])
            , test "succeeds for MM" <|
                \_ ->
                    Expect.equal (timeStrToHrsMinsSecs "31") (Ok [ 31 ])
            ]
        ]
