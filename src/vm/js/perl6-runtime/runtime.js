var nqp = require('nqp-runtime');
var CodeRef = require('nqp-runtime/code-ref');
var op = {};

var Scalar, True, False, Int, Num, Str, Code;

op.p6settypes = function(types) {
  Scalar = types.content.get('Scalar');
  True = types.content.get('True');
  False = types.content.get('False');
  Int = types.content.get('Int');
  Num = types.content.get('Num');
  Str = types.content.get('Str');
  Code = types.content.get('Code');
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

op.p6captureouters2 = function(ctx, capList, target) {
  var cf = (target.outerCtx || closure.forcedOuter);

  if (cf === null) {
    return capList;
  }

  var elems = capList.$$elems();

  for (var i = 0; i < elems; i++) {
      var codeObj = capList.$$atpos(i);

      var closure = codeObj.$$getattr(Code, "$!do");

      var ctxToDiddle = (closure.outerCtx || closure.forcedOuter);
      ctxToDiddle.$$outer = cf;
  }

  return capList;
};

op.p6captureouters = function(ctx, capList) {

  var elems = capList.$$elems();

  for (var i = 0; i < elems; i++) {
      var codeObj = capList.$$atpos(i);
      var closure = codeObj.$$getattr(Code, "$!do");
      var ctxToDiddle = (closure.outerCtx || closure.forcedOuter);
      ctxToDiddle.$$outer = ctx;
  }

  return capList;
};

op.p6capturelex = function(ctx, codeObj) {

  var closure = codeObj.$$getattr(Code, "$!do");
  var wantedStaticInfo = closure.staticCode.outerCodeRef;

  if (ctx.codeRef().staticCode === wantedStaticInfo) {
    closure.forcedOuter = ctx;
  } else if (ctx.$$outer.codeRef().staticCode === wantedStaticInfo) {
    closure.forcedOuter = ctx.$$outer;
  }

  return codeObj;
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

  this.STable.addInternalMethod('$$iscont', function() {
    return 1;
  });
};

RakudoScalar.prototype.serialize = function(cursor) {
  console.log('serializing rakudo_scalar');
};

RakudoScalar.prototype.deserialize = function(cursor) {
  console.log('* deserializing rakudo_scalar');
};

RakudoScalar.prototype.name = 'rakudo_scalar';

containerSpecs.rakudo_scalar = RakudoScalar;

nqp.loadOps({op: op});
module.exports = null;

