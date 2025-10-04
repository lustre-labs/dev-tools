let ws;
let reconnectAttempts = 0;

const maxReconnectAttempts = 10;
const reconnectInterval = 1000;

if (window.sessionStorage.getItem("hotreload")) {
  console.log("[lustre] Page reloaded by hot reload");
  window.sessionStorage.removeItem("hotreload");
}

window.addEventListener("error", (event) => {
  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(
      JSON.stringify({
        type: "client-error",
        message: event.message,
        filename: event.filename,
        lineno: event.lineno,
        colno: event.colno,
        stack: event.error ? event.error.stack : "",
      }),
    );
  }
});

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
        window.sessionStorage.setItem("hotreload", "true");
        window.location.reload();
        return;
      }

      case "asset-update": {
        const path = data.asset;
        const selector = `[src*="${path}"], [href*="${path}"]`;

        console.log(`[lustre] Asset updated ${path}`);

        for (const el of document.querySelectorAll(selector)) {
          if (el.hasAttribute("href")) {
            el.href = el.href.split("?")[0] + "?t=" + Date.now();
          }

          if (el.src) {
            el.src = el.src.split("?")[0] + "?t=" + Date.now();
          }
        }

        removeError();

        return;
      }

      case "error": {
        const message = data.message;

        showError(message);

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

function showError(message) {
  // Create and show error dialog
  const dialog = document.createElement("dialog");

  dialog.style.cssText = `
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    max-width: 80vw;
    max-height: 80vh;
    width: max-content;
    padding: 0;
    border: none;
    border-radius: 8px;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
    background: #1a1a1a;
    color: #fff;
    font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
    z-index: 10000;
  `;

  const header = document.createElement("div");

  header.style.cssText = `
    background: #dc2626;
    padding: 16px 20px;
    display: flex;
    justify-content: space-between;
    align-items: center;
    border-radius: 8px 8px 0 0;
  `;

  header.innerHTML = `
    <h3 style="margin: 0; font-size: 16px; font-weight: 600;">Build Error</h3>
    <button onclick="this.closest('dialog').close()" style="
      background: none;
      border: none;
      color: white;
      font-size: 20px;
      cursor: pointer;
      padding: 0;
      width: 24px;
      height: 24px;
      display: flex;
      align-items: center;
      justify-content: center;
    ">×</button>
  `;

  const content = document.createElement("div");

  content.style.cssText = `
    padding: 20px;
    overflow: auto;
    max-height: 60vh;
  `;

  const pre = document.createElement("pre");

  pre.style.cssText = `
    margin: 0;
    white-space: pre-wrap;
    word-wrap: break-word;
    font-size: 14px;
    line-height: 1.4;
    color: #f3f4f6;
  `;

  pre.textContent = message;

  content.appendChild(pre);
  dialog.appendChild(header);
  dialog.appendChild(content);

  // Remove existing error dialog if present
  removeError();

  dialog.setAttribute("data-lustre-error", "true");
  document.body.appendChild(dialog);
  dialog.showModal();

  // Close on backdrop click
  dialog.addEventListener("click", (e) => {
    if (e.target === dialog) {
      dialog.remove();
    }
  });

  // Close on escape key
  dialog.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      dialog.remove();
    }
  });

  // Remove dialog when closed
  dialog.addEventListener("close", () => {
    dialog.remove();
  });
}

function removeError() {
  const existingDialog = document.querySelector("dialog[data-lustre-error]");

  if (existingDialog) {
    existingDialog.remove();
  }
}
