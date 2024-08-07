//
//  SignInView.swift
//  runo
//
//  Created by Chip Jackson on 12/26/23.
//

import AuthenticationServices
import Combine
import SwiftUI
import WebKit
import KeychainAccess

struct SignInView: View {
    @State private var isSignedIn = false

    init() {
        if Keychain(service: "chipjacks.runo")["runoUserToken"] != nil {
            _isSignedIn = State(initialValue: true)
        } else {
            _isSignedIn = State(initialValue: false)
        }
    }

    var body: some View {
        NavigationView {
            if isSignedIn {
                ContentView()
            } else {
                Button("Authenticate") {
                    guard let authURL = URL(string: ApiClient.HOST + "/users/sign_in?v=ios0.1") else { return }

                    let authenticationSession = ASWebAuthenticationSession(
                        url: authURL,
                        callbackURLScheme: "runo"
                    ) { callbackURL, error in
                        guard error == nil, let callbackURL = callbackURL else { return }

                        //   runo://auth?token=1234
                        let queryItems = URLComponents(string: callbackURL.absoluteString)?.queryItems
                        let token = queryItems?.filter({ $0.name == "token" }).first?.value
                        if let token = token {
                            // Store token in Keychain
                            let keychain = Keychain(service: "chipjacks.runo")
                            keychain["runoUserToken"] = token
                            isSignedIn = true
                        }
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
