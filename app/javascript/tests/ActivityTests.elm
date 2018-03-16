module ActivityTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import TestHelpers exposing (..)
import Activity exposing (..)


suite : Test
suite =
    describe "Activity"
        [ describe "#groupByType"
            [ test "groups activities by type" <|
                \_ -> groupByType activities
                    |> List.map (\a -> List.map .type_ a)
                    |> Expect.equal [[Run, Run, Run], [Ride]]
            -- , test "uses earliest date" <|
            --     \_ -> groupByType activities
            --         |> List.map .date
            --         |> Expect.equal [date 1, date 2]
            ]
        ]