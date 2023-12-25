import { Elm } from "../Page/Account.elm";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");
  const flagsElement = document.getElementById("elm-flags");
  const flags = flagsElement ? JSON.parse(flagsElement.dataset.flags) : {};

  const app = Elm.Page.Account.init({ node: target, flags });
});
