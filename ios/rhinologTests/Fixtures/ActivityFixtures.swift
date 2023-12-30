//
//  ActivityFixtures.swift
//  rhinologTests
//
//  Created by Chip Jackson on 12/29/23.
//

import Foundation
@testable import rhinolog

struct ActivityFixtures {
    func EasyHour() throws -> Activity {
        return try loadFixture("easyHour")
    }

    func EightMile() throws -> Activity {
        return try loadFixture("eightMile")
    }

    func EightMileHour() throws -> Activity {
        return try loadFixture("eightMileHour")
    }

    func TempoThursday() throws -> Activity {
        return try loadFixture("tempoThursday")
    }

    func CruiseMiles() throws -> Activity {
        return try loadFixture("cruiseMiles")
    }

    private func loadFixture(_ name: String) throws -> Activity {
        let fileURL = Bundle(for: rhinologTests.self).url(forResource: name, withExtension: "json")
        let data = try Data(contentsOf: fileURL!)
        let decoder = JSONDecoder()
        let decodedData = try decoder.decode(Activity.self, from: data)
        return decodedData
    }
}
