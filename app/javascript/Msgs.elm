module Msgs exposing (..)

import Models exposing (HumanAPIActivity, RataDie)
import Date exposing (Date)
import RemoteData exposing (WebData)


type Msg
    = AActivitiesMsg ActivitiesMsg


type ActivitiesMsg
    = GotActivities RataDie (WebData (List HumanAPIActivity))
    | FetchActivities Date
