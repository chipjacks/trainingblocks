import { Elm } from "../Page/Settings.elm";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");
  const flagsElement = document.getElementById("elm-flags");
  const flags = flagsElement ? JSON.parse(flagsElement.dataset.flags) : {};

  const app = Elm.Page.Settings.init({ node: target, flags });

  customElements.define(
    "list-dnd",
    class extends HTMLElement {
      _handlePointerMove(event) {
        const dragged = document.getElementById("dragged-element");
        if (!dragged) {
          return;
        }

        dragged.hidden = true;
        let dropTarget = document.elementFromPoint(
          event.clientX,
          event.clientY
        );
        dragged.hidden = false;
        dropTarget = dropTarget && dropTarget.closest(".drop-target");
        if (dropTarget) {
          let id = Number(dropTarget.dataset.dropId);
          app.ports.setDropTarget.send(id);
        }
      }

      connectedCallback() {
        this.addEventListener("pointermove", this._handlePointerMove);
      }
      disconnectedCallback() {
        this.removeEventListener("pointermove", this._handlePointerMove);
      }
    }
  );

  app.ports.setPointerCapture.subscribe(({ targetId, pointerId }) => {
    const target = document.getElementById(targetId);
    event.target.setPointerCapture(pointerId);
  });
});
