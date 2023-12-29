	//
//  ContentView.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/25/23.
//

import SwiftUI

struct ContentView:  View {
	@State private var activities: [Activity]?
	@State private var loadingError: Error?


	var body: some View {
		VStack(alignment: .leading, spacing: 16) {
			if let todayActivity = activities?.first(where: { $0.date == currentDate() }) {
				// Today Section
				VStack(alignment: .leading, spacing: 16) {
					Text("Today")
						.font(.title)
						.fontWeight(.bold)
					
					Text(todayActivity.description)
						.font(.headline)

					if let distanceInMeters = todayActivity.data.laps?.first?.distance {
						// Convert distance from meters to miles
						let distanceInMiles = Measurement(value: distanceInMeters, unit: UnitLength.meters).converted(to: .miles).value
						
						Text("Distance: \(String(format: "%.2f", distanceInMiles)) miles")
							.font(.subheadline)
					}
					
					if let paceInSeconds = todayActivity.data.laps?.first?.pace {
						// Convert pace from seconds to MM:SS format
						let paceInMinutes = Int(paceInSeconds) / 60
						let paceInSecondsRemainder = Int(paceInSeconds) % 60

						Text("Pace: \(String(format: "%d:%02d", paceInMinutes, paceInSecondsRemainder)) min/mile")
							.font(.subheadline)
					}
					
					Spacer()
					
					Button(action: {
						// Move to Tomorrow logic goes here
					}) {
						Text("Move to Tomorrow")
							.padding()
							.background(Color.blue)
							.foregroundColor(.white)
							.cornerRadius(10)
					}
				}
			} else if let error = loadingError {
				Text("Error: \(error.localizedDescription)")
					.foregroundColor(.red)
					.padding()
			} else {
				Text("Loading...")
					.padding()
			}
		}
		.onAppear {
			ApiClient.shared.requestActivities { result in
				switch result {
				case .success(let loadedActivities):
					activities = loadedActivities
				case .failure(let error):
					loadingError = error
				}
			}
		}
		.padding()
		.padding(.top)
	}
	
	
	private func currentDate() -> String {
		let dateFormatter = DateFormatter()
		dateFormatter.dateFormat = "yyyy-MM-dd"
		return dateFormatter.string(from: Date())
	}
}

#Preview {
    ContentView()
}
