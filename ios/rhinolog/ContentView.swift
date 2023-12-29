//
//  ContentView.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/25/23.
//

import SwiftUI
import WorkoutKit

struct ContentView: View {
    @State private var activities: [Activity]?
    @State private var loadingError: Error?
    @State var showPreview: Bool = false

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

                    Spacer()

                    Button(action: {
                        showPreview.toggle()
                    }) {
                        Text("Preview Workout")
                            .padding()
                            .background(Color.blue)
                            .foregroundColor(.white)
                            .cornerRadius(10)
                    }.workoutPreview(todayActivity.toWorkoutPlan(), isPresented: $showPreview)
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
                case let .success(loadedActivities):
                    activities = loadedActivities
                case let .failure(error):
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
