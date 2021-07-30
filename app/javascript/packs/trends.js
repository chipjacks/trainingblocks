import { Elm } from "../Page/Trends";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");
  const flagsElement = document.getElementById("elm-flags");
  const flags = flagsElement ? JSON.parse(flagsElement.dataset.flags) : {};

  const app = Elm.Page.Trends.init({ node: target, flags });
});
