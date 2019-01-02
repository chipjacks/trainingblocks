module ActivityTests exposing (suite)

import Activity exposing (Activity, ActivityType(..))
import Time exposing (Month(..))
import Date exposing (Date)
import Expect exposing (Expectation)
import Json.Decode as JD
import Test exposing (..)
import TestHelpers exposing (activities)


suite : Test
suite =
    let
        activityStr =
            """
            {
                "id": 1272606297,
                "external_id": "garmin_push_2328646805",
                "upload_id": 1379272893,
                "athlete": {
                    "id": 2456610
                },
                "name": "Morning Run",
                "distance": 17187.2,
                "moving_time": 5447,
                "elapsed_time": 5760,
                "total_elevation_gain": 244.2,
                "elev_high": 98.4,
                "elev_low": 2.6,
                "type": "Run",
                "start_date": "2017-11-11T15:45:28.000+00:00",
                "start_date_local": "2017-11-11T07:45:28.000+00:00",
                "timezone": "(GMT-08:00) America/Los_Angeles",
                "achievement_count": 8,
                "kudos_count": 2,
                "comment_count": 0,
                "athlete_count": 1,
                "photo_count": 0,
                "total_photo_count": 0,
                "map": {
                    "id": "a1272606297",
                    "summary_polyline": "maabHx|ajVzCb@jBiCbVr@xd@lQv^oC`OaGdHaMzBeNo@_SfAQlNlHF~G{EnJeBrq@lFzAef@bDhb@qArANfCjGdJYs@~HeC~Dx@tDcBrElAdByBbCwBfJpF|K\\tV~GvJbD`@vBeCvKeBL{FxDwD|@mNdFaYqGyJkR@yHaHIsGrFqGDkEnK}F_@wHqGl@kIvGqChLu@fMmC|D~@dE_BbFxAfBwFdLjE`Im@bEbB@O|LXiLy@qE{PkFe@}EhEuNpEwc@wHm@|B{n@g@kBbDiF~@kLqLwGcCAt@`SiHhV_FnFaMzDs]tCwLuFiMs@oKyFe_@y@"
                },
                "trainer": false,
                "commute": false,
                "manual": false,
                "private": false,
                "flagged": false,
                "average_speed": 3.155,
                "max_speed": 5.2,
                "has_kudoed": false
            }
        """

        activity =
            { id = "1272606297"
            , name = "Morning Run"
            , distance = 17187.2
            , movingTime = 5447
            , elapsedTime = 5760
            , totalElevationGain = 244.2
            , type_ = Run
            , startDate = Date.fromCalendarDate 2017 Nov 11
            , averageSpeed = 3.155
            , maxSpeed = 5.2
            }
    in
    describe "Activity"
        [ describe "#decoder"
            [ test "decodes activities" <|
                \_ ->
                    JD.decodeString Activity.decoder activityStr
                        |> Expect.equal (Ok activity)
            ]
        , describe "#groupByType"
            [ test "groups activities by type" <|
                \_ ->
                    Activity.groupByType activities
                        |> List.map (\a -> List.map .type_ a)
                        |> Expect.equal [ [ Run, Run, Run ] ]
            ]
        ]
