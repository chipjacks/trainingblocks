//
//  SignInView.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/26/23.
//

import SwiftUI
import WebKit
import Combine
import AuthenticationServices


struct SignInView: View {
	@State private var isSignedIn = false

	var body: some View {
		NavigationView {
			if isSignedIn {
				ContentView()
			} else {
				Button("Authenticate") {
					guard let authURL = URL(string: "http://localhost:3000/users/sign_in?v=ios0.1") else { return }

					let authenticationSession = ASWebAuthenticationSession(
						url: authURL,
						callbackURLScheme: "rhinolog"
					) { callbackURL, error in
						guard error == nil, let callbackURL = callbackURL else { return }

						//   rhinolog://auth?token=1234
						let queryItems = URLComponents(string: callbackURL.absoluteString)?.queryItems
						let token = queryItems?.filter({ $0.name == "token" }).first?.value
						UserDefaults.standard.set(token, forKey: "rhinologUserToken") // TODO use keychain?
						isSignedIn = true

					}
					
					let authenticationPresenter = AuthenticationPresenter()
					authenticationSession.presentationContextProvider = authenticationPresenter
					authenticationSession.start()
				}
			}
		}
	}
}

struct SignInView_Previews: PreviewProvider {
	static var previews: some View {
		SignInView()
	}
}

class AuthenticationPresenter: NSObject, ASWebAuthenticationPresentationContextProviding {
	// Conforming to ASWebAuthenticationPresentationContextProviding protocol
	func presentationAnchor(for session: ASWebAuthenticationSession) -> ASPresentationAnchor {
		return UIApplication.shared.windows.first!
	}
}
