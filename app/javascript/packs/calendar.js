import { Elm } from "../Page/Calendar";

document.addEventListener("DOMContentLoaded", () => {
  const target = document.getElementById("elm-main");
  const flagsElement = document.getElementById("elm-flags");
  const flags = flagsElement ? JSON.parse(flagsElement.dataset.flags) : {};

  const app = Elm.Page.Calendar.init({ node: target, flags });

  document.addEventListener("scroll", function (e) {
    app.ports.handleScroll.send(e);
  });

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

      if (scrollFail) {
        scrollToSelectedDate();
      }
    }
  });

  function handleCalendarScroll(event) {
    const calendar = document.scrollingElement;
    if (
      calendar.scrollTop < 100 ||
      calendar.scrollHeight - calendar.scrollTop === calendar.clientHeight
    ) {
      return;
    }
    let monthHeader = Array.from(
      document.getElementsByClassName("month-header")
    )
      .map((e) => ({ element: e, rect: e.getBoundingClientRect() }))
      .filter((r) => r.rect.top < 100)
      .pop();
    if (monthHeader) {
      app.ports.selectDateFromScroll.send(monthHeader.element.dataset.date);
    }
  }

  let scrollFail = false;
  function scrollToSelectedDate() {
    setTimeout(() => {
      try {
        document.removeEventListener("scroll", handleCalendarScroll);
      } catch (e) {
        scrollFail = true;
        return;
      }
      const element = document.getElementById("selected-date");
      if (element) {
        element.scrollIntoView();
      }
      setTimeout(() => {
        document.addEventListener("scroll", handleCalendarScroll);
      }, 500);
    }, 100);
  }

  app.ports.scrollToSelectedDate.subscribe(scrollToSelectedDate);

  document.addEventListener("gotpointercapture", (e) => {
    e.target.releasePointerCapture(e.pointerId);
  });
});
