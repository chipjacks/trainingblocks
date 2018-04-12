module ActivityTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Json.Decode as JD
import Date exposing (Date, Month(..))
import Date.Extra as Date
import TestHelpers exposing (activities)
import Activity exposing (Activity, ActivityType(..))


suite : Test
suite =
    let
        activityStr = """
            {
                "id": null,
                "user_id": null,
                "start_date": "2017-11-11T15:45:28.000Z",
                "duration": 5447,
                "distance": 17187.2,
                "completed": true,
                "external_id": "1272606297",
                "created_at": null,
                "updated_at": null,
                "type_": "Run",
                "name": "Morning Run"
            }
        """
        activity =
            { id = Nothing
            , name = "Morning Run"
            , distance = 17187.2
            , duration = 5447
            , type_ = Run
            , startDate = Date.fromParts 2017 Nov 11 7 45 28 0 
            , completed = True
            , externalId = Just "1272606297"
            }
    in
        describe "Activity" [
            describe "#decoder"
                [ test "decodes activities" <|
                    \_ -> JD.decodeString Activity.decoder activityStr
                        |> Expect.equal (Ok activity)
                ]
            , describe "#groupByType"
                [ test "groups activities by type" <|
                    \_ -> Activity.groupByType activities
                        |> List.map (\a -> List.map .type_ a)
                        |> Expect.equal [[Run, Run, Run]]
                ]
            ]