const H = require("@funkia/hareactive");

exports._mapStream = function _map(f, s) {
  return s.map(f);
}

exports._mapBehavior = exports._mapStream;

exports._filter = H.filter;

exports._combine = H.combine;

exports._keepWhen = H.keepWhen;

exports._scan = H.scan;

exports.sample = H.sample

exports._mapNow = exports._mapStream;

exports._applyNow = function(nf, na) {
  return na.ap(nf);
}

exports._pureNow = H.Now.of;

exports._bindNow = function(m, f) {
  return m.chain(f);
}
