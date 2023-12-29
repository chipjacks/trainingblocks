//
//  Activity.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/26/23.
//

import Foundation
import WorkoutKit
import HealthKit

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
		let laps: [Lap]?
		let planned: [Lap]?
		
		struct Lap: Codable {
			let type: String?
			let pace: Int?
			let distance: Double?
			let duration: Int?
			let elevationGain: Double?
			let completed: Bool?
		}
	}
}

func convertActivityToCustomWorkout(activity: Activity) -> CustomWorkout {
	let workoutActivityType = HKWorkoutActivityType.running
	let workoutLocationType = HKWorkoutSessionLocationType.unknown
	let workoutDisplayName = activity.description
	var workoutSteps: [IntervalBlock] = []

	if let laps = activity.data.planned {
		for lap in laps {
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
				step.alert = .speed(Double(milesPerHour - 0.1)...Double(milesPerHour + 0.1), unit: .milesPerHour, metric: .current)
			}
			let interval = IntervalStep(.work, step: step)
			let block = IntervalBlock(steps: [interval], iterations: 1)
			workoutSteps.append(block)
		}
	}

	let customWorkout = CustomWorkout(activity: workoutActivityType, location: workoutLocationType, displayName: workoutDisplayName, warmup: nil, blocks: workoutSteps, cooldown: nil)
	return customWorkout
}
