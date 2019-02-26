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

exports.selfie = H.selfie;

exports._keepWhen = H.keepWhen;

exports._accumFrom = H.accumFrom;

exports._scanFrom = H.scanFrom;

exports._stepperFrom = H.stepperFrom;

exports.shiftCurrent = H.shiftCurrent;

exports.time = H.time;

exports.timeFrom = H.timeFrom;

exports._switchTo = H.switchTo;

exports._switcherFrom = H.switcherFrom;

exports._changes = H.changes;

exports._toggleFrom = H.toggleFrom;

exports.moment = H.moment;

exports.integrateFrom = H.integrateFrom;

exports._logS = function(name, stream) {
  stream.log(name);
};

exports._logB = function(name, behavior) {
  behavior.log(name);
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
