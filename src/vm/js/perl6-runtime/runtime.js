var nqp = require('nqp-runtime');
var Null = nqp.Null;
var CodeRef = require('nqp-runtime/code-ref');
var op = {};

var Scalar, True, False, Int, Num, Str, Code, Mu, Any, ContainerDescriptor, Routine;

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
  Routine = types.content.get('Routine');
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
  return (obj === Null || obj.typeObject_) ? False : True;
};

op.p6typecheckrv = function(rv, routine, bypassType) {
  // STUB
  return rv;
};

function isRWScalar(check) {
  if (check._STable === Scalar._STable && !check.typeObject_) {
    let desc = check.$$getattr(Scalar, '$!descriptor');
    if (desc === Null) {
      return false;
    }
    return desc.$$getattr_i(ContainerDescriptor, '$!rw') !== 0;
  }
  return false;
}

op.p6decontrv = function(routine, cont) {
  if (isRWScalar(cont)) {
    let isRW = routine.$$getattr_i(Routine, '$!rw');
    if (isRW === 0) {
      let roCont = Scalar._STable.REPR.allocate(Scalar._STable);
      roCont.$$bindattr(Scalar, '$!value', cont.$$decont(null));
      return roCont;
    }
  } else if (cont._STable && cont._STable.REPR instanceof nqp.NativeRef) {
    let isRW = routine.$$getattr_i(Routine, '$!rw');
    if (isRW === 0) {
      return cont.$$decont();
    }
  }
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
  var cf = target.outerCtx;

  if (cf === Null) {
    return capList;
  }

  var elems = capList.$$elems();

  for (var i = 0; i < elems; i++) {
      var codeObj = capList.$$atpos(i);

      var closure = codeObj.$$getattr(Code, "$!do");

      var ctxToDiddle = closure.outerCtx;
      if (ctxToDiddle) {
        ctxToDiddle.$$outer = cf;
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
    closure.outerCtx = ctx;
    closure.capture(closure.staticCode.freshBlock());
  } else {
    /* HACK - workaround for rakudo bugs */
    console.log("HORRIBLE hack - p6capturelex will do nothing");
  }

  return codeObj;
};

op.p6capturelexwhere = function(ctx, codeObj) {
//  console.trace("p6captaturelexwhere");
  var closure = codeObj.$$getattr(Code, "$!do");
  var wantedStaticInfo = closure.staticCode.outerCodeRef;

  var find = ctx;

  while (find) {
      if (find.codeRef().staticCode === wantedStaticInfo) {
        closure.outerCtx = find;
        closure.capture(closure.staticCode.freshBlock());
        return codeObj;
      }
      find = find.$$caller;
  }
  console.log("HORRIBLE hack - p6capturelexwhere will do nothing");

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
    if (value.$$decont(ctx).$$istype(ctx, type) == 0) {
      ctx.die("Type check failed in binding");
    }
  }
  return value;
};

op.p6store = function(ctx, cont, value) {
  if (cont.$$assign) {
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

op.p6recont_ro = function(cont) {
  /* TODO - repack RW scalars
  if (isRWScalar(tc, gcx, cont)) {
      SixModelObject roCont = gcx.Scalar.st.REPR.allocate(tc, gcx.Scalar.st);
      roCont.bind_attribute_boxed(tc, gcx.Scalar, "$!value",
          RakudoContainerSpec.HINT_value,
          cont.st.ContainerSpec.fetch(tc, cont));
      return roCont;
  }*/
  return cont;
};

op.p6scalarfromdesc = function(desc) {
  if (desc === Null || desc.typeObject_)
      desc = defaultContainerDescriptor;

  let defVal = desc.$$getattr(ContainerDescriptor, '$!default');

  let cont = Scalar._STable.REPR.allocate(Scalar._STable);

  cont.$$bindattr(Scalar, '$!descriptor', desc);

  cont.$$bindattr(Scalar, '$!value', defVal);

  return cont;

};

op.p6decodelocaltime = function(sinceEpoch) {
  let date = new Date(sinceEpoch * 1000);

  return nqp.createArray([
    date.getSeconds(),
    date.getMinutes(),
    date.getHours(),
    date.getDate(),
    date.getMonth()+1,
    date.getFullYear()
  ]);
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
    this.$$bindattr(Scalar, '$!value', value);
  });

  this.STable.addInternalMethod('$$assign', function(ctx, value) {
    /* TODO - checking and WHENCE */
    this.$$bindattr(Scalar, '$!value', value);
  });

  this.STable.addInternalMethod('$$decont', function(ctx) {
    return this.$$getattr(Scalar, '$!value');
  });

  this.STable.addInternalMethod('$$iscont', function() {
    return 1;
  });

  this.STable.addInternalMethod('$$isrwcont', function() {
    if (this.typeObject_) return 0;
    var desc = this.$$getattr(Scalar, '$!descriptor', desc);
    return desc.$$getattr_i(ContainerDescriptor, '$!rw');
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

