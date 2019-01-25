const H = require("@funkia/hareactive");

function _map(f, s) {
  return s.map(f);
}

exports._filter = H.filter;

exports._split = H.split;

exports._applyS = H.apply;

exports._filterApply = H.filterApply;

exports._snapshot = H.snapshot;

exports._snapshotWith = H.snapshotWith;

exports._keepWhen = H.keepWhen;

exports._scan = H.scan;

exports._stepper = H.stepper;

exports._scanS = H.scanS;

exports.switchStream = H.switchStream;

exports.time = H.time;

exports.timeFrom = H.timeFrom;

exports._switchTo = H.switchTo;

exports._switcher = H.switcher;

exports.changes = H.changes;

exports._toggle = H.toggle;

exports.moment = H.moment;

exports._logS = function() {
  return function(name, stream) {
    stream.log(name);
    return stream;
  };
};

exports._logB = function(dict) {
  return function(name, behavior) {
    behavior.map(dict.show).log(name);
    return behavior;
  };
};

// Now

exports.sample = H.sample;

exports.plan = H.plan;

exports.sinkFuture = H.sinkFuture;

exports._resolveFuture = function(future, value) {
  return function() {
    future.resolve(value);
  };
};

exports._mapCbStream = H.mapCbStream;

exports._runNow = function(now) {
  return now.run();
}

exports._performMap = H.performMap;

exports._performMapFuture = H.performMap;
