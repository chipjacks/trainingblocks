
const VERSION = '0.1.2';

const cacheName = `rhinolog v${VERSION}`;

self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      return cache.addAll([
        '/icon.svg',
        '/icon@480w.png',
        '/manifest.json',
      ]);
    })
  );
  self.skipWaiting();
});

self.addEventListener('activate', function(event) {
  var cacheAllowlist = [cacheName];

  event.waitUntil(
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          if (cacheAllowlist.indexOf(cacheName) === -1) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});

self.addEventListener('fetch', function(event) {
  event.respondWith(
    caches.match(event.request).then(function(response) {
      if (response) {
        return response;
      }
      return fetch(event.request);
    })
  );
});
