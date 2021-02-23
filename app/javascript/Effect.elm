module Effect exposing (Effect(..), perform)

import Activity exposing (Activity)
import Api
import Date
import Msg exposing (Msg(..))
import Ports
import Random
import Result exposing (Result)
import Task exposing (Task)
import Time


type Effect
    = None
    | Batch (List Effect)
    | PostActivities
        (List Msg)
        { token : String
        , revision : String
        , orderUpdates : List ( String, Int )
        , activityUpdates : List ( String, Activity )
        }
    | GetActivities
    | DateToday (Date.Date -> Msg)
    | ScrollToSelectedDate
    | GenerateActivity (Activity -> Msg) (Random.Generator Activity)
    | StoreCmd Msg
    | Cmd (Cmd Msg)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        None ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map perform effects)

        PostActivities msgs { token, revision, orderUpdates, activityUpdates } ->
            Api.postActivities token revision orderUpdates activityUpdates
                |> Task.attempt (Posted msgs)

        GetActivities ->
            Task.attempt GotActivities Api.getActivities

        DateToday msg ->
            Task.perform msg Date.today

        ScrollToSelectedDate ->
            Ports.scrollToSelectedDate ()

        GenerateActivity msg generator ->
            Random.generate msg generator

        StoreCmd msg ->
            Task.perform (\_ -> msg) (Task.succeed ())

        Cmd cmd ->
            cmd
