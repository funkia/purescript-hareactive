const H = require("@funkia/hareactive");

exports._producerStream = H.producerStream;

exports.sinkStream = H.sinkStream;

exports._pushSink = function(a, sinkStream) {
  sinkStream.push(a);
}

exports.sinkStreamToStream = function(sinkStream) {
  return sinkStream;
}

exports._subscribe = function(cb, stream) {
  stream.subscribe(cb);
}

exports._fromFunction = H.fromFunction;
