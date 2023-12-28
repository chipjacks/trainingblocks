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
					guard let authURL = URL(string: "http://localhost:3000/users/sign_in") else { return }

					let authenticationSession = ASWebAuthenticationSession(
						url: authURL,
						callbackURLScheme: "rhinolog"
					) { callbackURL, error in
						
						// Handle the callback URL or error here
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
