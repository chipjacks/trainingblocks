//
//  ApiClient.swift
//  runo
//
//  Created by Chip Jackson on 12/26/23.
//

import Foundation

class ApiClient {
    static let shared = ApiClient()

    static let HOST = "https://runo.app"

    func requestActivities(completion: @escaping (Result<[Activity], Error>) -> Void) {
        guard let url = URL(string: ApiClient.HOST + "/activities") else {
            completion(.failure(ActivityLoadingError.invalidUrl))
            return
        }
        var request = URLRequest(url: url)

        // Set Authorization Header
        if let savedToken = UserDefaults.standard.string(forKey: "runoUserToken") {
            request.setValue("Bearer \(savedToken)", forHTTPHeaderField: "Authorization")
        } else {
            // Handle the case where the token is not available
            completion(.failure(ActivityLoadingError.missingToken))
            return
        }

        URLSession.shared.dataTask(with: request) { data, response, error in
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

    enum ActivityLoadingError: Error {
        case non200Response(HTTPURLResponse?)
        case invalidData
        case invalidUrl
        case missingToken
    }
}
