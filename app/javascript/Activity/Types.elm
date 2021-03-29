module Activity.Types exposing (Activity, ActivityData, ActivityType(..), Completion(..), Effort(..), Id, LapData(..), RaceDistance(..), Seconds)

import Date exposing (Date)
import Pace exposing (Pace)


type alias Activity =
    { id : Id
    , date : Date
    , description : String
    , data : ActivityData
    , laps : Maybe (List LapData)
    }


type LapData
    = Individual ActivityData
    | Repeats Int (List ActivityData)


type alias ActivityData =
    { activityType : ActivityType
    , duration : Maybe Seconds
    , completed : Completion
    , pace : Maybe Pace
    , distance : Maybe Meters
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


type alias Meters =
    Int


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


type Effort
    = Easy
    | Moderate
    | Hard
