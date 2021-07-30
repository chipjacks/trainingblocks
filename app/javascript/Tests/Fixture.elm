module Tests.Fixture exposing (activities)

import Activity.Types exposing (..)
import Date exposing (fromRataDie)


activities =
    [ { date = fromRataDie 737920
      , description = "Morning Run"
      , id = "5279089993"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 10299.8016, distanceUnits = Just Miles, duration = Just 3063, effort = Just Easy, emoji = Nothing, pace = Just 480, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737921
      , description = "Drills, strides, hill repeats"
      , id = "7337448450"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 3701.4912, distanceUnits = Just Miles, duration = Just 1020, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing }, Individual { activityType = Other, completed = Completed, distance = Nothing, distanceUnits = Nothing, duration = Just 600, effort = Just Moderate, emoji = Nothing, pace = Nothing, race = Nothing }, Repeats 4 [ { activityType = Run, completed = Completed, distance = Just 160.9344, distanceUnits = Just Miles, duration = Just 45, effort = Just Moderate, emoji = Nothing, pace = Just 368, race = Nothing }, { activityType = Run, completed = Completed, distance = Nothing, distanceUnits = Just Miles, duration = Just 90, effort = Just Easy, emoji = Nothing, pace = Nothing, race = Nothing } ], Individual { activityType = Run, completed = Completed, distance = Just 3701.4912, distanceUnits = Just Miles, duration = Just 600, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing } ]
      , planned = [ Individual { activityType = Run, completed = Planned, distance = Just 3218.688, distanceUnits = Just Miles, duration = Just 900, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing }, Repeats 2 [ { activityType = Run, completed = Planned, distance = Just 5471.7696, distanceUnits = Just Miles, duration = Just 1200, effort = Just Moderate, emoji = Nothing, pace = Just 353, race = Nothing }, { activityType = Run, completed = Planned, distance = Just 643.7376, distanceUnits = Just Miles, duration = Just 180, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing } ], Individual { activityType = Run, completed = Planned, distance = Just 3218.688, distanceUnits = Just Miles, duration = Just 900, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing } ]
      }
    , { date = fromRataDie 737922
      , description = "GSF workout "
      , id = "6941246971"
      , laps = [ Individual { activityType = Other, completed = Completed, distance = Nothing, distanceUnits = Nothing, duration = Just 3900, effort = Just Moderate, emoji = Nothing, pace = Nothing, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737923
      , description = "Easy run + drills and strides"
      , id = "3841381632"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 13679.424, distanceUnits = Just Miles, duration = Just 4121, effort = Just Easy, emoji = Just "slightly_smiling_face", pace = Just 485, race = Nothing }, Individual { activityType = Other, completed = Completed, distance = Nothing, distanceUnits = Nothing, duration = Just 600, effort = Just Easy, emoji = Nothing, pace = Nothing, race = Nothing } ]
      , planned = [ Individual { activityType = Run, completed = Planned, distance = Just 15127.833600000002, distanceUnits = Just Miles, duration = Just 4200, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing }, Repeats 4 [ { activityType = Run, completed = Planned, distance = Just 160.9344, distanceUnits = Just Miles, duration = Just 30, effort = Just Moderate, emoji = Nothing, pace = Just 292, race = Nothing }, { activityType = Run, completed = Planned, distance = Just 160.9344, distanceUnits = Just Miles, duration = Just 60, effort = Just Easy, emoji = Nothing, pace = Just 448, race = Nothing } ] ]
      }
    , { date = fromRataDie 737923
      , description = "Core"
      , id = "9240466699"
      , laps = [ Individual { activityType = Other, completed = Completed, distance = Nothing, distanceUnits = Nothing, duration = Just 1200, effort = Just Moderate, emoji = Nothing, pace = Nothing, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737924
      , description = "Lunch Run"
      , id = "5297320528"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 8690.457600000002, distanceUnits = Just Miles, duration = Just 3088, effort = Just Easy, emoji = Nothing, pace = Just 573, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737924
      , description = "Pull ups, push ups, box jumps"
      , id = "6934657023"
      , laps = [ Individual { activityType = Other, completed = Completed, distance = Nothing, distanceUnits = Nothing, duration = Just 600, effort = Just Moderate, emoji = Nothing, pace = Nothing, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737925
      , description = "Lunch Run"
      , id = "5303180059"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 11426.3424, distanceUnits = Just Miles, duration = Just 3370, effort = Just Easy, emoji = Nothing, pace = Just 476, race = Nothing } ]
      , planned = []
      }
    , { date = fromRataDie 737926
      , description = ""
      , id = "5440809066"
      , laps = [ Individual { activityType = Run, completed = Completed, distance = Just 23657.3568, distanceUnits = Just Miles, duration = Just 6624, effort = Just Easy, emoji = Nothing, pace = Just 452, race = Nothing } ]
      , planned = [ Individual { activityType = Run, completed = Planned, distance = Just 25427.6352, distanceUnits = Just Miles, duration = Just 6000, effort = Just Moderate, emoji = Nothing, pace = Just 398, race = Nothing } ]
      }
    ]
