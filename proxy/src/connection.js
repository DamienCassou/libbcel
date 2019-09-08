const request = require("request-promise-native");
let oauth = require("./oauth");

let auth_token = undefined;

let account_id = undefined;

function set_account_id(anId) {
  account_id = anId;
}

async function generate_auth_token({ client_id, client_secret, port }) {
  auth_token = oauth.get_auth_token({ client_id, client_secret, port });
  await auth_token;
  return {};
}

async function get({ url, path, params }) {
  if (!account_id) {
    throw new Error("No account_id set.");
  }

  let uri = url || `https://3.basecampapi.com/${account_id}/${path}`;

  let response = await _doGet({ uri, params });
  let result = response.body;

  // https://github.com/basecamp/bc3-api/#pagination. If there are
  // more results, fetch the next batch and ignore the others:
  if (response.headers.link) {
    const link = response.headers.link;
    const semiColonPos = link.indexOf(">");
    const next = link.substring(1, semiColonPos);

    if (next) {
      let responseNext = await _doGet({ uri: next });
      result = [...result, ...responseNext.body];
    }
  }

  return result;
}

async function _doGet({ uri, params }) {
  if (!auth_token) {
    throw new Error(
      "No auth token set yet. Consider using generate_auth_token() first."
    );
  }

  return request.get({
    json: true,
    uri,
    resolveWithFullResponse: true,
    headers: {
      "User-Agent": "basecampel (damien@cassou.me)",
      Authorization: `Bearer ${(await auth_token).access_token}`
    },
    qs: params
  });
}

module.exports = { get, generate_auth_token, set_account_id };
