const H = require("@funkia/hareactive");

exports._subscribe = function(cb, stream) {
  stream.subscribe(cb);
}

exports._fromFunction = H.fromFunction;
