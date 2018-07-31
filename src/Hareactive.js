const H = require("@funkia/hareactive");

function apply(f, a) {
  return a.ap(f);
}

function bind(mf, f) {
  return mf.chain(f);
}

exports._memptyStream = H.empty;

function _map(f, s) {
  return s.map(f);
}

// Future

exports._mapFuture =_map;

exports._appendFuture = function(fa, fb) {
  return fa.combine(fb);
};

exports._applyFuture = function(f, a) {
  return a.ap(f);
}

exports._pureFuture = H.Future.of;

exports._bindFuture = function(mf, f) {
  return mf.chain(f);
}

// Stream

exports._mapStream = _map;

exports._mapBehavior = _map;

exports._applyBehavior = apply;

exports._bindBehavior = bind;

exports._pureBehavior = H.Behavior.of;

exports._filter = H.filter;

exports._apply = H.apply;

exports._filterApply = H.filterApply;

exports._snapshot = H.snapshot;

exports._snapshotWith = H.snapshotWith;

exports._combine = H.combine;

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

// Now

exports.sample = H.sample;

exports._mapNow = exports._mapStream;

exports._applyNow = apply;

exports._pureNow = H.Now.of;

exports._bindNow = bind;

exports.plan = H.plan;

exports.liftEffectNow = function(eff) {
  // This relies on the fact that the function given to `map` is only executed
  // once the `Now` is being run. This ensures that the eff is only executed
  // inside another eff (originating in `runNow`).
  return H.Now.of(undefined).map(function(_) { return eff(); });
}
