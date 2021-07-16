import { Elm } from "../Page/Trends";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");

  const flags = {};

  const app = Elm.Page.Trends.init({ node: target, flags });
});
