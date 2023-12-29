//
//  Activity.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/26/23.
//

import Foundation
import HealthKit
import WorkoutKit

struct ActivitiesContainer: Decodable {
    let activities: [Activity]

    private enum CodingKeys: String, CodingKey {
        case activities
    }
}

struct Activity: Codable {
    let id: String
    let date: String
    let description: String
    let data: ActivityData

    struct ActivityData: Codable {
        let laps: [LapOrRepeat]?
        let planned: [LapOrRepeat]?

        enum LapOrRepeat: Codable {
            case lap(Lap)
            case repeats(Repeats)

            init(from decoder: Decoder) throws {
                let container = try decoder.singleValueContainer()
                if let repeats = try? container.decode(Repeats.self) {
                    self = .repeats(repeats)
                } else if let lap = try? container.decode(Lap.self) {
                    self = .lap(lap)
                } else {
                    throw DecodingError.typeMismatch(LapOrRepeat.self, DecodingError.Context(codingPath: decoder.codingPath, debugDescription: "Failed to decode LapOrRepeat"))
                }
            }

            func encode(to encoder: Encoder) throws {
                var container = encoder.singleValueContainer()
                switch self {
                case let .lap(lap):
                    try container.encode(lap)
                case let .repeats(repeats):
                    try container.encode(repeats)
                }
            }

            func lap() -> Lap? {
                switch self {
                case let .lap(lap):
                    return lap
                default:
                    return nil
                }
            }

            func repeats() -> Repeats? {
                switch self {
                case let .repeats(repeats):
                    return repeats
                default:
                    return nil
                }
            }
        }

        struct Lap: Codable {
            let type: String?
            let pace: Int?
            let distance: Double?
            let duration: Int?
            let effort: Effort?
            let elevationGain: Double?
            let completed: Bool?

            enum Effort: String, Codable {
                case easy = "Easy"
                case moderate = "Moderate"
                case hard = "Hard"
            }
        }

        struct Repeats: Codable {
            let repeats: Int
            let laps: [Lap]
        }
    }

    func toWorkoutPlan() -> WorkoutPlan? {
        guard let workout = toCustomWorkout() else {
            return nil
        }
        let workoutPlan = WorkoutPlan(.custom(workout), id: uuid()!)
        return workoutPlan
    }

    private func toCustomWorkout() -> CustomWorkout? {
        let workoutActivityType = HKWorkoutActivityType.running
        let workoutLocationType = HKWorkoutSessionLocationType.unknown
        let workoutDisplayName = description
        var workoutSteps: [IntervalBlock] = []
        guard let laps = data.planned else {
            return nil
        }

        let (warmup, mainLaps, cooldown) = extractWarmupAndCooldown(laps: laps) ?? (nil, laps, nil)
        for lapOrRepeat in mainLaps {
            switch lapOrRepeat {
            case let .lap(lap):
                let interval = lapToIntervalStep(lap: lap)
                let block = IntervalBlock(steps: [interval], iterations: 1)
                workoutSteps.append(block)
            case let .repeats(repeats):
                let intervals = repeats.laps.map { lapToIntervalStep(lap: $0) }
                let block = IntervalBlock(steps: intervals, iterations: repeats.repeats)
                workoutSteps.append(block)
            }
        }

        let customWorkout = CustomWorkout(activity: workoutActivityType, location: workoutLocationType, displayName: workoutDisplayName, warmup: warmup, blocks: workoutSteps, cooldown: cooldown)
        return customWorkout
    }

    private func lapToIntervalStep(lap: ActivityData.Lap) -> IntervalStep {
        var step: WorkoutStep
        if let duration = lap.duration {
            step = WorkoutStep(goal: .time(Double(duration), .seconds))
        } else if let distance = lap.distance {
            step = WorkoutStep(goal: .distance(distance, .meters))
        } else {
            step = WorkoutStep(goal: .open)
        }
        if let paceSecondsPerMile = lap.pace {
            let milesPerHour = 1 / Double(paceSecondsPerMile) * 60 * 60
            step.alert = .speed(Double(milesPerHour - 0.1) ... Double(milesPerHour + 0.1), unit: .milesPerHour, metric: .current)
        }
        let interval = IntervalStep(lap.effort == .easy ? .recovery : .work, step: step)
        return interval
    }

    private func extractWarmupAndCooldown(laps: [ActivityData.LapOrRepeat]) -> (WorkoutStep, [ActivityData.LapOrRepeat], WorkoutStep)? {
        guard laps.count >= 3, let firstLap = laps.first?.lap(), let lastLap = laps.last?.lap() else {
            return nil
        }

        guard firstLap.effort == .easy, lastLap.effort == .easy else {
            return nil
        }

        let warmup = lapToIntervalStep(lap: firstLap)
        let cooldown = lapToIntervalStep(lap: lastLap)

        return (warmup.step, laps.dropFirst().dropLast(), cooldown.step)
    }

    func uuid() -> UUID? {
        guard id.count == 10 else { return nil }
        let uuidStr = "00000000-0000-0000-0000-00\(id)"
        return UUID(uuidString: uuidStr)
    }
}

#if DEBUG
    extension Activity {
        func _toCustomWorkout() -> CustomWorkout? {
            return toCustomWorkout()
        }
    }
#endif
