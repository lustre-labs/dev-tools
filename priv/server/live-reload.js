let socket = null;

function connect() {
  socket = new WebSocket(`ws://${window.location.host}/lustre-dev-tools`);

  socket.onmessage = (event) => {
    if (event.data === "reload") {
      window.location.reload();
    }
  };

  // If the dev server goes down we'll continue to try to reconnect
  // every 5 seconds. If the user needs to kill the server for some
  // reason, this means the page will restore live reload without a
  // refresh.
  socket.onclose = () => {
    socket = null;
    setTimeout(() => connect(), 5000);
  };

  socket.onerror = () => {
    socket = null;
    setTimeout(() => connect(), 5000);
  };
}

connect();
