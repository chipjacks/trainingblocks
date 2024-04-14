//
//  ContentView.swift
//  runo
//
//  Created by Chip Jackson on 12/25/23.
//

import SwiftUI
import WorkoutKit

struct ContentView: View {
    @State var activities: [Activity]?
    @State private var loadingError: Error?
    @State var showPreview: Bool = false
    @State var showPreviewFor: UUID? = nil
    @State var authorizationState: WorkoutScheduler.AuthorizationState = .notDetermined
    @State var scheduledWorkouts: [ScheduledWorkoutPlan] = []

    var body: some View {
        NavigationStack {
            if let loadedActivities = activities {
                List {
                    Section("Today") {
                        ForEach(loadedActivities.filter({ $0.date == currentDate() }), id: \.self) { activity in
                            activityView(activity: activity)
                        }
                    }
                    .padding()
                    .padding(.top)
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
        .navigationTitle("Upcoming Activities")
        .task {
            await update()
        }.refreshable {
            await update()
        }
    }

    @ViewBuilder
    private func activityView(activity: Activity) -> some View {
        Text(activity.description)
            .font(.headline)
        if let workoutPlan = activity.toWorkoutPlan() {
            HStack {
                Button(action: {
                    if showPreviewFor != nil {
                        showPreviewFor = nil
                        showPreview = false
                    } else {
                        showPreviewFor = activity.uuid()
                        showPreview = true
                    }
                }) {
                    Text("Preview")
                        .padding()
                        .background(Color.blue)
                        .foregroundColor(.white)
                        .cornerRadius(10)
                }.workoutPreview(workoutPlan, isPresented: $showPreview)

                Button(action: {
                    Task {
                        await schedule(workout: workoutPlan, date: activity.getDate())
                    }
                }) {
                    Text("Schedule")
                        .padding()
                        .background(Color.blue)
                        .foregroundColor(.white)
                        .cornerRadius(10)
                }
            }
        }
    }

    private func currentDate() -> String {
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yyyy-MM-dd"
        #if DEBUG
            let date = dateFormatter.date(from: "2023-12-28")!
        #else
            let date = Date()
        #endif
        return dateFormatter.string(from: date)
    }

    private func update(force: Bool = false) async {
        if force || authorizationState != .authorized {
            authorizationState = await WorkoutScheduler.shared.requestAuthorization()
        }
        scheduledWorkouts = await WorkoutScheduler.shared.scheduledWorkouts
        ApiClient.shared.requestActivities { result in
            switch result {
            case let .success(loadedActivities):
                activities = loadedActivities
            case let .failure(error):
                loadingError = error
            }
        }
    }

    private func schedule(workout: WorkoutPlan,
                          date: Date) async {
        let nextDateComponents = Calendar.autoupdatingCurrent.dateComponents(in: .autoupdatingCurrent, from: date)
        await WorkoutScheduler.shared.schedule(workout, at: nextDateComponents)

        scheduledWorkouts.append(ScheduledWorkoutPlan(workout, date: nextDateComponents))
    }
}

#Preview {
    do {
        return ContentView(activities: [try ActivityFixtures().EightMile()])
    } catch {
        return Text("failed to load activity")
    }
}
