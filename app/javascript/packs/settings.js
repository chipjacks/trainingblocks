import { Elm } from "../Pages/Settings";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");

  const flags = {};

  const app = Elm.Pages.Settings.init({ node: target, flags });
});
