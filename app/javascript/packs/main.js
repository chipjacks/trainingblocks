// Run this example by adding <%= javascript_pack_tag "main" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import { Elm } from '../Main';

document.addEventListener('DOMContentLoaded', () => {
  const target = document.getElementById('elm-main')
  const existing = document.getElementById('elm-overlay')

  const app = Elm.Main.init(target);

  if('serviceWorker' in navigator) {
    navigator.serviceWorker
      .register('/service-worker.js', { scope: './' });

    let refreshing;
    navigator.serviceWorker.addEventListener('controllerchange', function () {
      if (refreshing) return;
      const conf = window.confirm('An update is available. Reload?')
      if (conf) {
        window.location.reload();
        refreshing = true;
      }
    });
  }

  window.localStorage.setItem('loadDate', Date.now());
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'visible') {
      const AUTO_RELOAD_DAILY_MS = 60 * 1000 * 60 * 24;
      if (navigator.standalone && (Date.now() - window.localStorage.getItem('loadDate') > AUTO_RELOAD_DAILY_MS)) {
        // manual reload because serviceWorker.update() not working for iOS PWA.
        window.localStorage.setItem('loadDate', Date.now());
        window.location.reload();
      };

      if (scrollFail) {
        scrollToSelectedDate();
      }
    }
  });

  function handleCalendarScroll(event) {
    const calendar = document.getElementById('calendar');
    if (calendar.scrollTop < 100 || (calendar.scrollHeight - calendar.scrollTop === calendar.clientHeight)) {
      return;
    }
    let monthHeader = Array.from(document.getElementsByClassName('month-header'))
      .map(e => ({ element: e, rect: e.getBoundingClientRect() }))
      .filter(r => r.rect.top < 100)
      .pop()
    if (monthHeader) {
      app.ports.selectDateFromScroll.send(monthHeader.element.dataset.date);
    }
  };

  let scrollFail = false;
  function scrollToSelectedDate() {
    setTimeout(() => {
      try {
        document.getElementById('calendar').removeEventListener('scroll', handleCalendarScroll);
      } catch (e) {
        scrollFail = true;
        return;
      }
      const element = document.getElementById('selected-date')
      if (element) {
        element.scrollIntoView();
      }
      setTimeout(() => {
        document.getElementById('calendar').addEventListener('scroll', handleCalendarScroll);
      }, 500);
    }, 100);
  }

  app.ports.scrollToSelectedDate.subscribe(scrollToSelectedDate);

  document.addEventListener('gotpointercapture', (e) => {
    e.target.releasePointerCapture(e.pointerId)
  });
})
