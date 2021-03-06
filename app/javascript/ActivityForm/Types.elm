module ActivityForm.Types exposing (ActivityForm, FieldError(..), ValidatedFields)

import Activity.Types exposing (Activity)
import Date exposing (Date)


type alias ActivityForm =
    { activity : Activity
    , date : Maybe Date
    , description : String
    , validated : ValidatedFields
    , laps : ( Int, List Activity.Types.LapData )
    , repeat : Maybe ( Int, List Activity.Types.ActivityData )
    , repeats : Maybe String
    , activityType : Activity.Types.ActivityType
    , duration : ( String, String, String )
    , completed : Activity.Types.Completion
    , pace : String
    , race : Maybe Activity.Types.RaceDistance
    , effort : Maybe Activity.Types.Effort
    , emoji : String
    , emojiSearch : String
    }


type alias ValidatedFields =
    { date : Result FieldError Date
    , repeats : Result FieldError Int
    , duration : Result FieldError Int
    , pace : Result FieldError Int
    , emoji : Result FieldError String
    }


type FieldError
    = MissingError
    | ParseError
    | ValueError
