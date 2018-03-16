module TestHelpers exposing (..)

import Date exposing (Month(..))
import Date.Extra as Date exposing (fromCalendarDate)
import Activity exposing (..)

date : Int -> Date.Date
date = Date.fromCalendarDate 2018 Jan

activity : Activity.Model
activity = Model Run (date 1) 1 20

activities : List Activity.Model
activities =
    [ Model Run (date 1) 1 30
    , Model Run (date 2) 3 20
    , Model Ride (date 2) 3 45
    , Model Run (date 3) 2 10
    ]