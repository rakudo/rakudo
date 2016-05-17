var nqp = require("nqp-runtime");
var op = {};
var perl6Types;
op.p6settypes = function(types) {
  p6Types = types.content;
  return types;
};

op.p6bool = function(value) {
  return value ? p6Types.get('True') : p6Types.get('False');
};

op.p6typecheckrv = function(rv, routine, bypassType) {
  // STUB
  return rv;
};

op.p6decontrv = function(rountine, contr) {
  // STUB
  return rv;
};

nqp.loadOps({op:op});
module.exports = null;

