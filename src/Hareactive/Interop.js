const H = require("@funkia/hareactive");

exports._producerStream = H.producerStream;

exports.sinkStream = H.sinkStream;

exports._pushSink = function(a, sinkStream) {
  sinkStream.push(a);
}

exports._writerBehavior = function(a, behavior) {
  behavior.push(a);
}

exports.sinkStreamToStream = function(sinkStream) {
  return sinkStream;
}

exports.readBehavior = function(b) {
  return b.at();
}

exports._fromFunction = H.fromFunction;

exports._producerBehavior = H.producerBehaviorFromFunction;

exports._subscribe = function(cb, stream) {
  stream.subscribe(cb);
}

exports._observe = H.observe;
