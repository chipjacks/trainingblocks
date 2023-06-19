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

        Effect.PostActivities _ _ ->
            SimulatedEffect.Cmd.none

        Effect.GetActivities ->
            SimulatedEffect.Task.attempt Msg.GotActivities
                (SimulatedEffect.Task.succeed ( "rev", [] ))

        Effect.GetSettings ->
            SimulatedEffect.Task.attempt Msg.GotSettings
                (SimulatedEffect.Task.succeed (Just { paces = [], raceDistance = Activity.Types.FiveK, raceDuration = 60 * 17, level = 44, showTime = True }))

        Effect.FetchEmojis ->
            SimulatedEffect.Task.attempt Msg.FetchedEmojis
                (SimulatedEffect.Task.succeed [])

        Effect.DateToday toMsg ->
            SimulatedEffect.Task.perform toMsg
                (SimulatedEffect.Task.succeed
                    (Date.fromCalendarDate 2020 Time.Jan 1)
                )

        Effect.GenerateActivity msg _ ->
            SimulatedEffect.Task.perform msg
                (SimulatedEffect.Task.succeed
                    (Activity "1234567890"
                        (Date.fromCalendarDate 2020 Time.Jan 1)
                        ""
                        [ Activity.Types.Individual Activity.initActivityData ]
                        []
                        Nothing
                    )
                )

        Effect.StoreCmd msg ->
            SimulatedEffect.Task.perform (\_ -> msg)
                (SimulatedEffect.Task.succeed ())

        _ ->
            SimulatedEffect.Cmd.none
