const H = require("@funkia/hareactive");

function apply(f, a) {
  return a.ap(f);
}

function bind(mf, f) {
  return mf.chain(f);
}

exports._memptyStream = H.empty;

exports._mapStream = function _map(f, s) {
  return s.map(f);
}

exports._mapBehavior = exports._mapStream;

exports._applyBehavior = apply;

exports._bindBehavior = bind;

exports._pureBehavior = H.Behavior.of;

exports._filter = H.filter;

exports._combine = H.combine;

exports._keepWhen = H.keepWhen;

exports._scan = H.scan;

exports._stepper = H.stepper;

exports._scanS = H.scanS;

exports.switchStream = H.switchStream;

exports.sample = H.sample

exports._mapNow = exports._mapStream;

exports._applyNow = apply;

exports._pureNow = H.Now.of;

exports._bindNow = bind;
