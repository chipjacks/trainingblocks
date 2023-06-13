module Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), DistanceUnits(..), Effort(..), Id, LapData(..), RaceDistance(..), Seconds)

import Date exposing (Date)


type alias Activity =
    { id : Id
    , date : Date
    , description : String
    , laps : List LapData
    , planned : List LapData
    , importId : Maybe String
    }


type LapData
    = Individual ActivityData
    | Repeats Int (List ActivityData)


type alias ActivityData =
    { activityType : ActivityType
    , duration : Maybe Seconds
    , completed : Completion
    , pace : Maybe Int
    , distance : Maybe Float
    , distanceUnits : Maybe DistanceUnits
    , race : Maybe RaceDistance
    , effort : Maybe Effort
    , emoji : Maybe String
    }


type Completion
    = Completed
    | Planned


type ActivityType
    = Run
    | Other


type alias Id =
    String


type alias Seconds =
    Int


type DistanceUnits
    = Miles
    | Kilometers
    | Meters
    | Yards


type RaceDistance
    = FiveK
    | EightK
    | FiveMile
    | TenK
    | FifteenK
    | TenMile
    | TwentyK
    | HalfMarathon
    | TwentyFiveK
    | ThirtyK
    | Marathon
    | OtherDistance


type Effort
    = Easy
    | Moderate
    | Hard
