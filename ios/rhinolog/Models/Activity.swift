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
		let laps: [Lap]
		
		struct Lap: Codable {
			let type: String
			let pace: Int
			let distance: Double
			let duration: Int
			let elevationGain: Double
			let completed: Bool
		}
	}
}
enum ActivityLoadingError: Error {
	case non200Response(HTTPURLResponse?)
	case invalidData
	case invalidUrl
}

func loadActivities(completion: @escaping (Result<[Activity], Error>) -> Void) {
	guard let url = URL(string: "http://localhost:3000/activities") else {
		completion(.failure(ActivityLoadingError.invalidUrl))
		return
	}

	URLSession.shared.dataTask(with: url) { data, response, error in
		guard let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 else {
			completion(.failure(ActivityLoadingError.non200Response(response as? HTTPURLResponse)))
			return
		}

		guard let data = data, error == nil else {
			completion(.failure(ActivityLoadingError.invalidData))
			return
		}

		do {
			let container = try JSONDecoder().decode(ActivitiesContainer.self, from: data)
			completion(.success(container.activities))
		} catch {
			print("Error decoding JSON: \(error)")
			completion(.failure(error))
		}
	}.resume()
}
