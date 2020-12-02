// Run this example by adding <%= javascript_pack_tag "main" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  const target = document.getElementById('elm-main')
  const existing = document.getElementById('elm-overlay')

  if (existing) {
    null
  } else {
    Elm.Main.embed(target)
  }
})
