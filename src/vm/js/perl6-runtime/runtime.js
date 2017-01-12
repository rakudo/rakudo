var nqp = require('nqp-runtime');
var Null = nqp.Null;
var CodeRef = require('nqp-runtime/code-ref');
var op = {};

var Scalar, True, False, Int, Num, Str, Code, Mu, Any, ContainerDescriptor;

var defaultContainerDescriptor;

op.p6settypes = function(types) {
  Scalar = types.content.get('Scalar');
  True = types.content.get('True');
  False = types.content.get('False');
  Int = types.content.get('Int');
  Num = types.content.get('Num');
  Str = types.content.get('Str');
  Code = types.content.get('Code');
  Mu = types.content.get('Mu');
  Any = types.content.get('Any');
  ContainerDescriptor = types.content.get('ContainerDescriptor');

  defaultContainerDescriptor = ContainerDescriptor._STable.REPR.allocate(ContainerDescriptor._STable);

  defaultContainerDescriptor.$$bindattr(ContainerDescriptor, '$!of', Mu);

  defaultContainerDescriptor.$$bindattr_s(ContainerDescriptor, '$!name', "<element>");
  defaultContainerDescriptor.$$bindattr_i(ContainerDescriptor, '$!rw', 1);
  defaultContainerDescriptor.$$bindattr(ContainerDescriptor, '$!default', Any);

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
  boxed.$$setInt(int);
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

  if (cf === Null) {
    return capList;
  }

  var elems = capList.$$elems();

  for (var i = 0; i < elems; i++) {
      var codeObj = capList.$$atpos(i);

      var closure = codeObj.$$getattr(Code, "$!do");

      var ctxToDiddle = (closure.outerCtx || closure.forcedOuter);
      if (ctxToDiddle) {
        ctxToDiddle.$$outer = cf;
        if (ctxToDiddle.closuresUsingThis) {
          for (var closure of ctxToDiddle.closuresUsingThis) {
            let closureCtx = closure.outerCtx;
            let updatedCtxs = [];
            while (updatedCtxs.length < closure.staticCode.closureTemplate.length) {
              updatedCtxs.unshift(closureCtx);
              closureCtx = closureCtx.$$outer;
            }
            closure.capture(closure.staticCode.closureTemplate.apply(null, updatedCtxs));
          }
        }
      } else {
        console.log("can't diddle", closure);
      }
  }

  return capList;
};

op.p6capturelex = function(ctx, codeObj) {
  var closure = codeObj.$$getattr(Code, "$!do");
  var wantedStaticInfo = closure.staticCode.outerCodeRef;

  if (ctx.codeRef().staticCode === wantedStaticInfo) {
    closure.forcedOuter = ctx;
    closure.outerCtx = ctx;
  } else if (ctx.$$outer.codeRef().staticCode === wantedStaticInfo) {
    closure.outerCtx = ctx;
    closure.forcedOuter = ctx.$$outer;
  }

  return codeObj;
};

op.p6var = function(cont) {
  if (cont.$$iscont && cont.$$iscont()) {
    var wrapper = Scalar._STable.REPR.allocate(Scalar._STable);
    wrapper.$$bindattr(Scalar, '$!value', cont);
    return wrapper;
  } else {
    return cont;
  }
}

op.p6bindassert = function(ctx, value, type) {
  if (type !== Mu) {
    if (nqp.op.istype(ctx, value.$$decont(ctx), type) == 0) {
      ctx.die("Type check failed in binding");
    }
  }
  return value;
};

op.p6store = function(ctx, cont, value) {
  if (cont) {
    cont.$$assign(ctx, value.$$decont(ctx));
  } else {
    if (!cont.STORE) {
      // TODO throw typed exception X::Assignment::RO
      ctx.die("Cannot assign to a non-container");
    } else {
      cont.STORE(ctx, null, cont, value);
    }
  }
  return cont;
};

op.p6argvmarray = function(ctx, args) {
  var array = [];
  for (var i=2; i < args.length; i++) {
    array[i-2] = nqp.op.hllizefor(ctx, args[i], 'perl6');
  }
  return nqp.createArray(array);
};

op.p6scalarfromdesc = function(desc) {
  console.log("p6scalarfromdesc");

  if (desc === Null || desc.typeObject_)
      desc = defaultContainerDescriptor;

  let defVal = desc.$$getattr(ContainerDescriptor, '$!default');

  let cont = Scalar._STable.REPR.allocate(Scalar._STable);

  cont.$$bindattr(Scalar, '$!descriptor', desc);

  cont.$$bindattr(Scalar, '$!value', defVal);

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
    return this.$$bindattr(Scalar, '$!value', value);
  });

  this.STable.addInternalMethod('$$assign', function(ctx, value) {
    /* TODO - checking and WHENCE */
    return this.$$bindattr(Scalar, '$!value', value);
  });

  this.STable.addInternalMethod('$$decont', function(ctx) {
    return this.$$getattr(Scalar, '$!value');
  });

  this.STable.addInternalMethod('$$getInt', function(ctx) {
    return this.$$decont().$$getInt();
  });

  this.STable.addInternalMethod('$$getNum', function(ctx) {
    return this.$$decont().$$getNum();
  });

  this.STable.addInternalMethod('$$getStr', function(ctx) {
    return this.$$decont().$$getStr();
  });

  this.STable.addInternalMethod('$$iscont', function() {
    return 1;
  });
};




RakudoScalar.prototype.serialize = function(cursor) {
  /* No data to serialize. */
};

RakudoScalar.prototype.deserialize = function(cursor) {
  /* No data to deserialize. */
};

RakudoScalar.prototype.name = 'rakudo_scalar';

containerSpecs.rakudo_scalar = RakudoScalar;

nqp.loadOps({op: op});
module.exports = null;

