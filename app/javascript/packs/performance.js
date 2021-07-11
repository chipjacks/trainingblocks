import { Elm } from "../Page/Performance";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");

  const flags = {};

  const app = Elm.Page.Performance.init({ node: target, flags });
});
