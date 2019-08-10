const connection = require("../connection");
const InvalidMessage = require("../InvalidMessage");

const handler = async (data = {}) => {
  switch (data.action) {
    case "find":
      return find(data);
    case "get":
      return get(data);
    default:
      throw new InvalidMessage(`Missing action`);
  }
};

const find = async ({ status }) => {
  return connection.get({
    path: "/projects.json",
    params: { status }
  });
};

const get = async ({ project_id } = {}) => {
  if (!project_id) {
    throw new InvalidMessage("Missing project_id");
  }

  return connection.get({ path: `/projects/${project_id}` });
};

module.exports = handler;
