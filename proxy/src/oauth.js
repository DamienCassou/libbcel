const { promisify } = require("util");
const exec = promisify(require("child_process").exec);

const express = require("express");
const request = require("request-promise-native");

function backchannel_request({ client_id, client_secret, redirect_uri, code }) {
  return request.post({
    json: true,
    uri: "https://launchpad.37signals.com/authorization/token",
    qs: {
      type: "web_server",
      client_id: client_id,
      redirect_uri: redirect_uri,
      client_secret: client_secret,
      code
    }
  });
}

function openWebBrowser({ client_id, redirect_uri }) {
  let uri = `https://launchpad.37signals.com/authorization/new?type=web_server&client_id=${client_id}&redirect_uri=${redirect_uri}`;
  return promisify(exec)(`xdg-open '${uri}'`);
}

function get_auth_token({ client_id, client_secret, port = 9321 }) {
  let redirect_uri = `http://localhost:${port}`;
  let server;

  return new Promise(resolve => {
    const app = express();

    app.get("/", (req, res) => {
      let code = req.query.code;

      backchannel_request({
        client_id,
        client_secret,
        redirect_uri,
        code
      }).then(authTokenData => {
        server.close();
        resolve(authTokenData);
      });

      res.end();
    });

    server = app.listen(port, () => {
      openWebBrowser({ client_id, redirect_uri });
    });

    return server;
  });
}

module.exports = {
  get_auth_token
};
