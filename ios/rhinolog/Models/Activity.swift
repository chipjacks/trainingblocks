//
//  Activity.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/26/23.
//

import Foundation

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
		
		struct Lap: Codable {
			let type: String
			let pace: Int?
			let distance: Double?
			let duration: Int?
			let elevationGain: Double?
			let completed: Bool?
		}
	}
}
