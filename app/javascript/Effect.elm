module Effect exposing (Effect(..), perform)

import Activity.Types exposing (Activity)
import Api
import Date
import EmojiData.Fetch
import Msg exposing (Msg(..), StoreData)
import Ports
import Random
import Store.History exposing (History)
import Task exposing (Task)
import Uuid
import Http


type Effect
    = None
    | Batch (List Effect)
    | PostActivities
        (History Msg StoreData)
        { revision : String
        , orderUpdates : List ( String, Int )
        , activityUpdates : List ( String, Activity )
        }
    | GetActivities
    | GetSettings
    | FetchEmojis
    | ReportError (Task Http.Error Uuid.Uuid)
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

        PostActivities msgs { revision, orderUpdates, activityUpdates } ->
            Api.postActivities revision orderUpdates activityUpdates
                |> Task.attempt (Posted msgs)

        GetActivities ->
            Task.attempt GotActivities Api.getActivities

        GetSettings ->
            Task.attempt GotSettings Api.getSettings

        FetchEmojis ->
            Task.attempt FetchedEmojis EmojiData.Fetch.task

        ReportError task ->
            Task.attempt ReportedError task

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
