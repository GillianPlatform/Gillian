var Module = function (id, dirname) {
  this.id = id;
  this.filename = id;
  this.dirname = dirname;
  this.status = "NOT_LOADED";
  this.exports = {};
}

var _cache = {};

function require(id) {
  var module = _cache[id];
  if (module.status === "NOT_LOADED") {
    module.load(module.exports, module, module.filename, module.dirname);
  }
  return module.exports;
}

var _module;
