var nqp = require('nqp-runtime');
var op = {};

var Scalar, True, False, Int, Num, Str;

op.p6settypes = function(types) {
  Scalar = types.content.get('Scalar');
  True = types.content.get('True');
  False = types.content.get('False');
  Int = types.content.get('Int');
  Num = types.content.get('Num');
  Str = types.content.get('Str');
  return types;
};

op.p6bool = function(value) {
  return value ? True : False;
};

op.p6definite = function(obj) {
  return obj.typeObject_ ? False : True;
};

op.p6typecheckrv = function(rv, routine, bypassType) {
  // STUB
  return rv;
};

op.p6decontrv = function(rountine, cont) {
  // STUB
  return cont;
};

op.p6box_i = function(int) {
  var repr = Int._STable.REPR;
  var boxed = repr.allocate(Int._STable);
  boxed.$$setInt(int.value);
  return boxed;
};

op.p6box_n = function(num) {
  var repr = Num._STable.REPR;
  var boxed = repr.allocate(Num._STable);
  boxed.$$setNum(num);
  return boxed;
};

op.p6box_s = function(str) {
  var repr = Str._STable.REPR;
  var boxed = repr.allocate(Str._STable);
  boxed.$$setStr(str);
  return boxed;
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

