const package = require("../package.json");
const semver = require("semver");
const connection = require("./connection");
const InvalidMessage = require("./InvalidMessage");

const handler = async (data = {}) => {
  switch (data.action) {
    case "version":
      return version(data);
    case "get":
      return connection.get(data);
    case "generate_auth_token":
      return connection.generate_auth_token(data);
    default:
      throw new InvalidMessage(`Missing action`);
  }
};

const version = async () => {
  return {
    version: {
      major: semver.major(package.version),
      minor: semver.minor(package.version),
      patch: semver.patch(package.version)
    }
  };
};

module.exports = handler;
