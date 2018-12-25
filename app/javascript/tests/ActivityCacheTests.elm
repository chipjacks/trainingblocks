module ActivityCacheTests exposing (suite)

import ActivityCache exposing (accessActivities, fetchActivities)
import Date exposing (Month(..))
import Date.Extra as Date
import Dict
import Expect exposing (Expectation)
import RemoteData exposing (RemoteData(..))
import Test exposing (..)
import TestHelpers exposing (activity)


suite : Test
suite =
    let
        startDate =
            Date.fromCalendarDate 2018 Jan 1

        endDate =
            Date.add Date.Month 3 startDate

        jan15 =
            Date.fromCalendarDate 2018 Jan 15

        loadedModel =
            Date.range Date.Month 1 startDate endDate
                |> List.map
                    (\d ->
                        ( Date.toRataDie d
                        , Success
                            (Date.range Date.Day 1 d (Date.add Date.Month 1 d)
                                |> List.map (\d -> activity (Date.day d) 4 30)
                            )
                        )
                    )
                |> Dict.fromList
                |> ActivityCache.Model
    in
    describe "ActivityCache"
        [ describe "#fetchActivities"
            [ test "sets all cached months to loading" <|
                \_ ->
                    fetchActivities ActivityCache.initModel startDate endDate
                        |> Tuple.first
                        |> .cache
                        |> Dict.values
                        |> Expect.equal [ Loading, Loading, Loading ]
            ]
        , describe "#accessActivities"
            [ test "returns activites if they have all been loaded" <|
                \_ ->
                    accessActivities loadedModel jan15 (Date.add Date.Week 1 jan15)
                        |> Expect.equal
                            (Date.range Date.Day 1 jan15 (Date.add Date.Week 1 jan15)
                                |> List.map (\d -> activity (Date.day d) 4 30)
                                |> RemoteData.succeed
                            )
            ]
        ]
