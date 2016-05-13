var nqp = require("nqp-runtime");
var op = {};
var perl6Types;
op.p6settypes = function(types) {
  p6Types = types.content;
  return types;
};
op.p6bool = function(value) {
  return value ? p6Types.get('True') : p6Types.get('False');
}
nqp.loadOps({op:op});
module.exports = null;
