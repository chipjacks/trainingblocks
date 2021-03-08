module ActivityForm.Types exposing (ActivityForm, FieldError(..), Selection, ValidatedFields)

import Activity.Types exposing (Activity, ActivityData, ActivityType, Completion, Effort, LapData, RaceDistance)
import Date exposing (Date)


type alias ActivityForm =
    { activity : Activity
    , date : Maybe Date
    , description : String
    , validated : ValidatedFields
    , laps : Selection LapData
    , repeat : Maybe (Selection ActivityData)
    , repeats : Maybe String
    , activityType : ActivityType
    , duration : ( String, String, String )
    , completed : Completion
    , pace : String
    , race : Maybe RaceDistance
    , effort : Maybe Effort
    , emoji : String
    , emojiSearch : String
    }


type alias Selection a =
    ( Int, List a )


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
