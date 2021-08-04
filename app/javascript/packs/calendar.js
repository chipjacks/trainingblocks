import { Elm } from "../Page/Calendar";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");
  const flagsElement = document.getElementById("elm-flags");
  const flags = flagsElement ? JSON.parse(flagsElement.dataset.flags) : {};

  const app = Elm.Page.Calendar.init({ node: target, flags });

  window.localStorage.setItem("loadDate", Date.now());
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") {
      const AUTO_RELOAD_DAILY_MS = 60 * 1000 * 60 * 24;
      if (
        navigator.standalone &&
        Date.now() - window.localStorage.getItem("loadDate") >
          AUTO_RELOAD_DAILY_MS
      ) {
        window.localStorage.setItem("loadDate", Date.now());
        window.location.reload();
      }
    }
  });

  document.addEventListener("gotpointercapture", (e) => {
    e.target.releasePointerCapture(e.pointerId);
  });

  customElements.define(
    "infinite-calendar",
    class extends HTMLElement {
      constructor() {
        super();
      }

      connectedCallback() {
        const handleScroll = this._handleCalendarScroll;
        document.addEventListener("scroll", function (e) {
          handleScroll(e);
        });
      }

      disconnectedCallback() {
        document.removeEventListener("scroll", this._handleCalendarScroll);
      }

      _handleCalendarScroll(event) {
        const calendar = document.scrollingElement;
        let monthHeader = Array.from(
          document.getElementsByClassName("month-header")
        )
          .map((e) => ({ element: e, rect: e.getBoundingClientRect() }))
          .filter((r) => r.rect.top < 100)
          .pop();
        if (monthHeader) {
          app.ports.selectDateFromScroll.send(monthHeader.element.dataset.date);
        }

        const loadMargin = 10;
        const debounceHandleScroll = debounceLeading(
          app.ports.handleScroll.send
        );
        if (calendar.scrollTop < loadMargin) {
          debounceHandleScroll(true);
        } else if (
          calendar.scrollTop >
          calendar.scrollHeight - calendar.clientHeight - 100 - loadMargin
        ) {
          debounceHandleScroll(false);
        }
      }
    }
  );

  customElements.define(
    "scroll-target",
    class extends HTMLElement {
      constructor() {
        super();
      }

      connectedCallback() {
        this.scrollIntoView();
      }
    }
  );
});

function debounceLeading(func, timeout = 300) {
  let timer;
  return (...args) => {
    if (!timer) {
      func.apply(this, args);
    }
    clearTimeout(timer);
    timer = setTimeout(() => {
      timer = undefined;
    }, timeout);
  };
}
