let ws;
let reconnectAttempts = 0;

const maxReconnectAttempts = 10;
const reconnectInterval = 1000;

function connect() {
  const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
  const url = `${protocol}//${window.location.host}/.lustre/ws`;

  ws = new WebSocket(url);

  ws.onopen = function () {
    console.log("[lustre] Connected to development server");
    reconnectAttempts = 0;
  };

  ws.onmessage = function (event) {
    const data = JSON.parse(event.data);

    switch (data.type) {
      case "reload": {
        window.location.reload();
        return;
      }

      case "asset-update": {
        const path = data.asset;
        const selector = `[src*="${path}"], [href*="${path}"]`;

        for (const el of document.querySelectorAll(selector)) {
          if (el.hasAttribute("href")) {
            el.href = el.href.split("?")[0] + "?t=" + Date.now();
          }

          if (el.src) {
            el.src = el.src.split("?")[0] + "?t=" + Date.now();
          }
        }

        return;
      }
    }
  };

  ws.onclose = function () {
    console.log("[lustre] Connection lost, attempting to reconnect...");

    if (reconnectAttempts < maxReconnectAttempts) {
      setTimeout(() => {
        reconnectAttempts++;
        connect();
      }, reconnectInterval);
    } else {
      console.log("[lustre] Max reconnection attempts reached");
    }
  };
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", connect);
} else {
  connect();
}
