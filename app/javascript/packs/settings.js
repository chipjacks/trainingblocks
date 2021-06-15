import { Elm } from "../Page/Settings";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");

  const flags = {};

  const app = Elm.Page.Settings.init({ node: target, flags });
});
