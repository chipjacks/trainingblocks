module ActivityForm.Types exposing (ActivityForm, ValidatedFields)

import Activity.Types exposing (Activity, ActivityData, ActivityType, Completion, DistanceUnits, Effort, LapData, RaceDistance)
import Date exposing (Date)
import Selection exposing (Selection)
import Validate exposing (FieldError)


type alias ActivityForm =
    { activity : Activity
    , editingLap : Bool
    , laps : Selection LapData
    , repeat : Maybe (Selection ActivityData)
    , validated : ValidatedFields
    , date : Maybe Date
    , description : String
    , repeats : Maybe String
    , activityType : ActivityType
    , duration : ( String, String, String )
    , completed : Completion
    , pace : String
    , distance : String
    , distanceUnits : DistanceUnits
    , elevationGain : Maybe Float
    , race : Maybe RaceDistance
    , effort : Maybe Effort
    , emoji : String
    , emojiSearch : String
    }


type alias ValidatedFields =
    { date : Result FieldError Date
    , repeats : Result FieldError Int
    , duration : Result FieldError Int
    , pace : Result FieldError Int
    , distance : Result FieldError Float
    }
