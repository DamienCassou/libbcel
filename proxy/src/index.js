const { createServer } = require("net");
const respond = require("./respond");

let socket;
let input = "";

function start({ port, account_id }) {
  require("./connection").set_account_id(account_id);

  let server = createServer(sock => {
    socket = sock;

    sock.on("error", e => {
      throw e;
    });

    sock.on("data", receive);
  });

  server.listen(port, () => {
    console.log(`Basecamp proxy listening on ${port}`);
  });
}

function stop() {
  send({ message: "closing" });
  process.exit();
}

function receive(str) {
  input += str;

  let linefeedPos = input.indexOf("\n");

  while (linefeedPos >= 0) {
    let data;
    try {
      data = JSON.parse(input.substring(0, linefeedPos));
      console.error("received: ", data);
      input = input.substring(++linefeedPos);
      linefeedPos = input.indexOf("\n");
    } catch (e) {
      error({ error: `Invalid JSON data received: "${input}"` });
      return;
    }

    if (!data.id) {
      error({ error: "Missing id from JSON data" });
      return;
    }

    respond(data)
      .then(value => {
        success({ id: data.id, payload: value });
      })
      .catch(err => {
        error({ id: data.id, error: err.message });
      });
  }
}

function send(data) {
  socket.write(`${JSON.stringify(data)}\n`);
}

function error({ id, error }) {
  send({ id, type: "error", payload: { error } });
}

function success({ id, payload = {} }) {
  send({ id, type: "success", payload });
}

module.exports = { start, stop, success, error, send };
