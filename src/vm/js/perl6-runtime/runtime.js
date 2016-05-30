var nqp = require('nqp-runtime');
var op = {};

var Scalar;
var True;
var False;

op.p6settypes = function(types) {
  Scalar = types.content.get('Scalar');
  True = types.content.get('True');
  False = types.content.get('False');
  return types;
};

op.p6bool = function(value) {
  return value ? True : False;
};

op.p6typecheckrv = function(rv, routine, bypassType) {
  // STUB
  return rv;
};

op.p6decontrv = function(rountine, cont) {
  // STUB
  return cont;
};

var containerSpecs = require('nqp-runtime/container-specs.js');

function RakudoScalar(STable) {
  this.STable = STable;
}

RakudoScalar.prototype.configure = function(conf) {
  this.setupSTable();
};

RakudoScalar.prototype.setupSTable = function() {
  this.STable.addInternalMethod('$$assignunchecked', function(ctx, value) {
    console.log('storing into rakudo_scalar unchecked');
    process.exit();
  });

  this.STable.addInternalMethod('$$assign', function(ctx, value) {
    console.log('storing into rakudo_scalar');
    process.exit();
  });

  this.STable.addInternalMethod('$$decont', function(ctx) {
    return this.$$getattr(Scalar, '$!value');
  });

  this.STable.addInternalMethod('$$getInt', function(ctx) {
    return this.$$decont().$$getInt();
  });
};

RakudoScalar.prototype.serialize = function(cursor) {
  console.log('serializing rakudo_scalar');
};

RakudoScalar.prototype.deserialize = function(cursor) {
  console.log('* deserializing rakudo_scalar');
  this.setupSTable();
};

RakudoScalar.prototype.name = 'rakudo_scalar';

containerSpecs.rakudo_scalar = RakudoScalar;

nqp.loadOps({op: op});
module.exports = null;

