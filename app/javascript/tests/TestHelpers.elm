module TestHelpers exposing (..)

import Date exposing (Month(..))
import Date.Extra as Date exposing (fromCalendarDate)
import Activity exposing (Activity, ActivityType(..))

activities : List Activity
activities =
    [ activity 1 4 30
    , activity 2 3 20
    , activity 3 4 45
    ]

activity : Int -> Float -> Int -> Activity
activity date miles minutes =
    { id = Just "1272606297"
    , name = "Morning Run"
    , distance = miles * 1609
    , duration = minutes * 60
    , type_ = Run
    , startDate = Date.fromParts 2018 Jan date 8 45 28 0 
    , completed = True
    , externalId = Just "garmin_push_123455"
    }