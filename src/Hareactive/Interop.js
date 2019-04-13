const H = require("@funkia/hareactive");

function id(a) {
  return a
}

exports._producerStream = H.producerStream;

exports.sinkStream = H.sinkStream;

exports._pushSink = function(a, sinkStream) {
  sinkStream.push(a);
}

exports.sinkStreamToStream = id;

exports._fromFunction = H.fromFunction;

exports._producerBehavior = H.producerBehaviorFromFunction;

exports._subscribe = function(cb, stream) {
  stream.subscribe(cb);
}

exports._observe = H.observe;
