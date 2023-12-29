//
//  ActivityTests.swift
//  rhinologTests
//
//  Created by Chip Jackson on 12/26/23.
//

import XCTest
@testable import rhinolog

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
		XCTAssertEqual(activity.data.laps?[0].type, "Run")
		XCTAssertEqual(activity.data.laps?[0].distance, 9022.2)
	}
}
