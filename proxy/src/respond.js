const meta = require("./meta");
const projects = require("./endpoints/projects");

const InvalidMessage = require("./InvalidMessage");

const respond = async (data = {}) => {
  switch (data.type) {
    case "meta":
      return meta(data);
    case "projects":
      return projects(data);
    default:
      throw new InvalidMessage(`Unknown type: ${data.type}`);
  }
};

module.exports = respond;
