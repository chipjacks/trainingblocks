module ActivityCacheTests exposing (..)

import Expect exposing (Expectation)
import Date exposing (Month(..))
import Date.Extra as Date
import Test exposing (..)
import Dict
import RemoteData exposing (RemoteData(..))
import ActivityCache exposing (fetchActivities, accessActivities)
import Activity exposing (ActivityType(..))


suite : Test
suite =
    let
        startDate = (Date.fromCalendarDate 2018 Jan 1)
        endDate = Date.add Date.Month 3 startDate
        jan15 = (Date.fromCalendarDate 2018 Jan 15)
        activity = (\d -> Activity.Model Run d 1 40)
        loadedModel = Date.range Date.Month 1 startDate endDate
            |> List.map (\d -> (d |> Date.toRataDie, Success (Date.range Date.Day 1 d (Date.add Date.Month 1 d) |> List.map activity) ) )
            |> Dict.fromList
            |> ActivityCache.Model
    in
        describe "ActivityCache"
            [ describe "#fetchActivities"
                [ test "sets all cached months to loading" <|
                    \_ -> fetchActivities ActivityCache.initModel startDate endDate
                        |> Tuple.first
                        |> .cache
                        |> Dict.values
                        |> Expect.equal [Loading, Loading, Loading]
                ]
            , describe "#accessActivities"
                [ test "returns activites if they have all been loaded" <|
                    \_ -> accessActivities loadedModel jan15 (Date.add Date.Week 1 jan15)
                        |> Expect.equal (Date.range Date.Day 1 jan15 (Date.add Date.Week 1 jan15) |> List.map activity |> RemoteData.succeed)
                ]
            ]