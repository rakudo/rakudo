use Test;

BEGIN unless $*VM ~~ "js" { plan 0; skip-rest "js only test"; done-testing; exit 0; };

plan 15;

is EVAL(:lang<JavaScript>, 'return 123'), 123, 'getting a number from js';
is EVAL(:lang<JavaScript>, 'return "simple string"'), "simple string", 'getting a string from js';

is EVAL(:lang<JavaScript>, 'return true'), True, 'getting true from js';
is EVAL(:lang<JavaScript>, 'return false'), False, 'getting false from js';
is EVAL(:lang<JavaScript>, 'return null'), Mu, 'getting null from js';

is EVAL(:lang<JavaScript>, q:to/END/).noArgs, 'noArgs return value', 'calling a method without arguments';
class Foo {
  noArgs() {
    return "noArgs return value";
  }
}
return new Foo();
END

is EVAL(:lang<JavaScript>, q:to/END/).withArgs("String arg", 41e0), 'This attr|String arg|41|string|number', 'calling a method with simple arguments';
class Foo {
  constructor(attr1) {
    this.attr1 = attr1;
  }

  withArgs(arg1, arg2) {
    return this.attr1 + '|' + arg1 + '|' + arg2 + '|' + (typeof arg1) + '|' + (typeof arg2);
  }
}
return new Foo('This attr');
END

my $obj = EVAL(:lang<JavaScript>, q:to/END/);
class Foo {
  simpleMethod() {
    return 'simple method call';
  }

  setAttr(value) {
    this.attr = value; 
  }

  getAttr() {
    return this.attr; 
  }

  getAttrFromArg(arg) {
    return arg.attr;
  }
}
return new Foo();
END

is $obj.simpleMethod, 'simple method call', 'can call the simplest of method on a js object in var';

$obj.setAttr('attr value');
is $obj.getAttr(), 'attr value', 'can set js attribute using accessor';

is $obj<attr>, 'attr value', 'can get the attribute directly';
$obj<attr> = 'attr value 2';

is $obj.getAttr(), 'attr value 2', 'can set the attribute directly';

is $obj.getAttrFromArg($obj), 'attr value 2', 'passing wrapped objects as arguments';

my $simple-function = EVAL(:lang<JavaScript>, q:to/END/);
return function(arg) {
  return arg * 10;
};
END

is $simple-function(66e0), 660e0, 'calling a js function with a Num argument';

my $pass-callback = EVAL(:lang<JavaScript>, q:to/END/);
  return function(cb) {
    return cb("World");
  };
END

is $pass-callback(-> str $value {"Hello " ~ $value}), "Hello World", "calling a p6 function from js";

lives-ok {
  EVAL(:lang<JavaScript>, q:to/END/)
  class Foo {}
  return new Foo();
  END
}, 'can sink a wrapped js object that does not have a sink method';
