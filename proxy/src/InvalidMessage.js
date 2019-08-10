module.exports = class InvalidMessage extends Error {
  constructor() {
    super(...arguments);
  }
};
