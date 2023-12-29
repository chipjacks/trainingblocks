//
//  ActivityFixtures.swift
//  rhinologTests
//
//  Created by Chip Jackson on 12/29/23.
//

import Foundation
@testable import rhinolog

struct ActivityFixtures {
	func TempoThursday() throws -> Activity {
		let fileURL = Bundle(for: rhinologTests.self).url(forResource: "tempoThursday", withExtension: "json")
		let data = try Data(contentsOf: fileURL!)
		let decoder = JSONDecoder()
		let decodedData = try decoder.decode(Activity.self, from: data)
		return decodedData
	}
}
