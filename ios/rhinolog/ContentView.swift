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
					
					Text("Description: \(todayActivity.description)")
						.font(.subheadline)
					
					Text("Distance: \(todayActivity.data.laps.first?.distance ?? 0) miles")
						.font(.subheadline)
					
					Text("Pace: \(todayActivity.data.laps.first?.pace ?? 0) min/mile")
						.font(.subheadline)
					
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
			loadActivities { result in
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
