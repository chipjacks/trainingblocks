//
//  ActivityTests.swift
//  rhinologTests
//
//  Created by Chip Jackson on 12/26/23.
//

import HealthKit
@testable import rhinolog
import WorkoutKit
import XCTest

class ActivityTests: XCTestCase {
    func testActivityDecoding() throws {
        // Given
        let json = """
        {
        	"activities": [
        		{
        			"id": "6986041384",
        			"date": "2022-04-14",
        			"order": null,
        			"description": "Lunch Run",
        			"data": {
        				"laps": [
        					{
        						"type": "Run",
        						"pace": 441,
        						"distance": 9022.2,
        						"duration": 2474,
        						"elevationGain": 70.6,
        						"completed": true
        					}
        				]
        			},
        			"user_id": 5,
        			"created_at": "2023-06-18T15:33:56.277Z",
        			"updated_at": "2023-06-18T15:33:56.277Z",
        			"import_id": "6986041384"
        		}
        	]
        }
        """

        let jsonData = json.data(using: .utf8)!

        let decoder = JSONDecoder()
        let container = try decoder.decode(ActivitiesContainer.self, from: jsonData)
        XCTAssertEqual(container.activities.count, 1)

        guard let activity = container.activities.first else {
            XCTFail("missing activity")
            return
        }
        XCTAssertEqual(activity.id, "6986041384")
        XCTAssertEqual(activity.date, "2022-04-14")
        XCTAssertEqual(activity.description, "Lunch Run")
        XCTAssertEqual(activity.data.laps?.count, 1)
        XCTAssertEqual(activity.data.laps?[0].lap()?.type, "Run")
        XCTAssertEqual(activity.data.laps?[0].lap()?.distance, 9022.2)
    }

    func testConvertActivityTimeGoal() throws {
        let activity = try ActivityFixtures().EasyHour()
        guard let workout = activity._toSingleGoalWorkout() else {
            XCTFail("missing workout")
            return
        }
        XCTAssertEqual(workout.goal, WorkoutGoal.time(60 * 60, .seconds))
    }

    func testConvertActivityDistanceGoal() throws {
        let activity = try ActivityFixtures().EightMile()
        guard let workout = activity._toSingleGoalWorkout() else {
            XCTFail("missing workout")
            return
        }
        XCTAssertEqual(workout.goal, WorkoutGoal.distance(8, .miles))
    }

    func testConvertActivityWithPace() throws {
        let activity = try ActivityFixtures().EightMileHour()
        guard let pacerWorkout = activity._toPacerWorkout() else {
            XCTFail("missing workout")
            return
        }
        XCTAssertEqual(pacerWorkout.distance.value, activity.data.planned?.first?.lap()?.distance)
        XCTAssertEqual(pacerWorkout.time.value, Double((activity.data.planned?.first?.lap()?.duration)!))
    }

    func testConvertActivityWithLaps() throws {
        let activity = try ActivityFixtures().TempoThursday()
        guard let customWorkout = activity._toCustomWorkout() else {
            XCTFail("missing workout")
            return
        }
        XCTAssertEqual(customWorkout.displayName, activity.description)
        XCTAssertEqual(customWorkout.blocks.count, activity.data.planned!.count)
    }

    func testConvertActivityWithRepeats() throws {
        let activity = try ActivityFixtures().CruiseMiles()
        guard let customWorkout = activity._toCustomWorkout() else {
            XCTFail("missing workout")
            return
        }
        XCTAssertEqual(customWorkout.displayName, activity.description)
        XCTAssertEqual(customWorkout.blocks.count, activity.data.planned!.count - 2)
        XCTAssertEqual(customWorkout.warmup?.goal, WorkoutGoal.time(Double((activity.data.planned?[0].lap()?.duration)!), .seconds))
        XCTAssertEqual(customWorkout.cooldown?.goal, WorkoutGoal.time(Double((activity.data.planned?.last!.lap()?.duration)!), .seconds))
        XCTAssertEqual(customWorkout.blocks[0].iterations, activity.data.planned?[1].repeats()?.repeats)
        XCTAssertEqual(customWorkout.blocks[0].steps[0].purpose, IntervalStep.Purpose.work)
        XCTAssertEqual(customWorkout.blocks[0].steps[1].purpose, IntervalStep.Purpose.recovery)
    }
}
