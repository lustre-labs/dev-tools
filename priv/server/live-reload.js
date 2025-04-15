let socket = null;
let timeout = null;

function connect() {
  let protocol = window.location.protocol === "https:" ? "wss" : "ws"
  socket = new WebSocket(`${protocol}://${window.location.host}/lustre-dev-tools`);

  if (timeout) {
    clearTimeout(timeout);
    timeout = null;
  }

  socket.onmessage = (event) => {
    const data = JSON.parse(event.data);
    switch (data.$) {
      case "reload":
        window.location.reload();
        break;
      case "error":
        replace_body_with_error(data.error);
        break;
    }
  };

  // If the dev server goes down we'll continue to try to reconnect
  // every 5 seconds. If the user needs to kill the server for some
  // reason, this means the page will restore live reload without a
  // refresh.
  socket.onclose = () => {
    socket = null;

    if (timeout) clearTimeout(timeout);
    if (!socket) timeout = setTimeout(() => connect(), 5000);
  };

  socket.onerror = () => {
    socket = null;

    if (timeout) clearTimeout(timeout);
    if (!socket) timeout = setTimeout(() => connect(), 5000);
  };
}

function replace_body_with_error(error) {
  document.body.innerHTML = `
<div style="display: flex; justify-content: center; border-top: 5px solid red;">
  <div style="font-family: monospace, monospace; white-space: pre-wrap; max-width: 100vw;">
    Error: ${error}
  </div>
</div>`;
}

connect();
