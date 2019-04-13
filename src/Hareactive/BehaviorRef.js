const H = require("@funkia/hareactive");

function id(a) {
  return a
}

exports._new = H.sinkBehavior;

exports._read = function(b) {
  return b.at();
}

exports.toBehavior = id;

exports._write = function(a, behavior) {
  behavior.push(a);
}
