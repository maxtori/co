const version = "1";
const cache = `co-${version}`;

const precache = [
  "./",
  "index.html",
  "ui.bc.js",
  "3ddice.js",
  "manifest.json",
  "assets/bs.css",
  "assets/pirata_one.ttf",
  "assets/parchemin_bg.webp",
  "assets/co_192.png",
  "https://cdn.jsdelivr.net/npm/bootstrap/dist/js/bootstrap.bundle.min.js",
  "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css",
  "https://unpkg.com/vue/dist/vue.global.prod.js",
];

self.addEventListener("install", (e) => {
  e.waitUntil(caches.open(cache).then((c) => c.addAll(precache)));
  self.skipWaiting();
});

self.addEventListener("activate", (e) => {
  e.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys.filter((k) => k !== cache).map((k) => caches.delete(k)),
        ),
      )
      .then(() => self.clients.claim()),
  );
});

self.addEventListener("fetch", (e) => {
  if (e.request.method !== "GET") return;
  e.respondWith(
    caches.open(cache).then(async (cache) => {
      const cached = await cache.match(e.request);
      const network = fetch(e.request)
        .then((resp) => {
          if (resp.ok || resp.type === "opaque")
            cache.put(e.request, resp.clone());
          return resp;
        })
        .catch(() => cached);
      return cached || network;
    }),
  );
});
