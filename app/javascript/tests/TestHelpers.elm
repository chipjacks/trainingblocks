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
    { id = "1272606297"
    , name = "Morning Run"
    , distance = miles * 1609
    , movingTime = minutes * 60
    , elapsedTime = minutes * 60
    , totalElevationGain = 244.2
    , type_ = Run
    , startDate = Date.fromParts 2018 Jan date 8 45 28 0 
    , startDateLocal = Date.fromParts 2017 Jan date 0 45 28 0 
    , averageSpeed = 4
    , maxSpeed = 5.2
    }