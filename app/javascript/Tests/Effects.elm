module Tests.Effects exposing (simulateEffects)

import Activity
import Activity.Types exposing (Activity)
import Api
import Date
import Effect exposing (Effect)
import Msg exposing (Msg)
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import SimulatedEffect.Task
import Time


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects effect =
    case effect of
        Effect.None ->
            SimulatedEffect.Cmd.none

        Effect.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)

        Effect.PostActivities msgs { token, revision, orderUpdates, activityUpdates } ->
            SimulatedEffect.Cmd.none

        Effect.GetActivities ->
            SimulatedEffect.Task.attempt Msg.GotActivities
                (SimulatedEffect.Http.task
                    { method = "GET"
                    , headers = []
                    , url = "activities"
                    , body = SimulatedEffect.Http.emptyBody
                    , timeout = Nothing
                    , resolver = SimulatedEffect.Http.stringResolver <| Api.getActivitiesResolver
                    }
                )

        Effect.DateToday toMsg ->
            SimulatedEffect.Task.perform toMsg
                (SimulatedEffect.Task.succeed
                    (Date.fromCalendarDate 2020 Time.Jan 1)
                )

        Effect.GenerateActivity msg generator ->
            SimulatedEffect.Task.perform msg
                (SimulatedEffect.Task.succeed
                    (Activity "1234567890"
                        (Date.fromCalendarDate 2020 Time.Jan 1)
                        ""
                        Activity.initActivityData
                        Nothing
                    )
                )

        Effect.StoreCmd msg ->
            SimulatedEffect.Task.perform (\_ -> msg)
                (SimulatedEffect.Task.succeed ())

        _ ->
            SimulatedEffect.Cmd.none
