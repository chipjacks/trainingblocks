//
//  SignInView.swift
//  rhinolog
//
//  Created by Chip Jackson on 12/26/23.
//

import SwiftUI
import WebKit
import Combine

struct SignInView: View {
	@State private var webViewNavigationPublisher = PassthroughSubject<WebViewNavigation, Never>()
	@State private var isSignedIn = false

	var body: some View {
		NavigationView {
			if isSignedIn {
				ContentView()
			} else {
				WebView(urlString: "http://localhost:3000/users/sign_in", navigationPublisher: webViewNavigationPublisher)
					.onReceive(webViewNavigationPublisher) { navigation in
						handleWebViewNavigation(navigation)
					}
			}
		}
	}

	private func handleWebViewNavigation(_ navigation: WebViewNavigation) {
		guard let signedIn = navigation.url?.absoluteString.contains("/calendar") else  {
			return
		}
		if signedIn {
			isSignedIn = true
		}
	}
}

struct WebView: UIViewRepresentable {
	let urlString: String
	let navigationPublisher: PassthroughSubject<WebViewNavigation, Never>

	func makeCoordinator() -> Coordinator {
		Coordinator(self)
	}

	func makeUIView(context: Context) -> WKWebView {
		let webView = WKWebView()
		webView.navigationDelegate = context.coordinator
		return webView
	}

	func updateUIView(_ uiView: WKWebView, context: Context) {
		if let url = URL(string: urlString) {
			let request = URLRequest(url: url)
			uiView.load(request)
		}
	}

	class Coordinator: NSObject, WKNavigationDelegate {
		var parent: WebView

		init(_ parent: WebView) {
			self.parent = parent
		}

		func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
			parent.navigationPublisher.send(WebViewNavigation(url: webView.url, isRedirected: false))
		}

		func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
			parent.navigationPublisher.send(WebViewNavigation(url: nil, isRedirected: false))
		}

		func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
			if let url = navigationAction.request.url, navigationAction.navigationType == .linkActivated {
				parent.navigationPublisher.send(WebViewNavigation(url: url, isRedirected: true))
				decisionHandler(.cancel)
			} else {
				decisionHandler(.allow)
			}
		}
	}
}

struct WebViewNavigation {
	let url: URL?
	let isRedirected: Bool
}

struct SignInView_Previews: PreviewProvider {
	static var previews: some View {
		SignInView()
	}
}
