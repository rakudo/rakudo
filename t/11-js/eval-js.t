use Test;

BEGIN unless $*VM ~~ "js" { plan 0; skip-rest "js only test"; done-testing; exit 0; };

plan 33;

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

my $wrapped-constructor = EVAL(:lang<JavaScript>, q:to/END/);
  class Foo {
    constructor(value) {
      this.value = value;
    }

    getValue() {
      return this.value;
    }

    static new(arg) {
      return 'js land new[' + arg + ']';
    }

    item(arg) {
      return 'js land item[' + arg + ']';
    }

    sink(arg) {
      return 'js land sink[' + arg + ']';
    }

    Bool(arg) {
      return 'js land Bool[' + arg + ']';
    }

    defined(arg) {
      return 'js land defined[' + arg + ']';
    }
  }
  return Foo;
END

my $instance = $wrapped-constructor.new('Passed Value');

is($instance.getValue, 'Passed Value', 'can use .new to create js objects');

is($wrapped-constructor.new(:INTERNAL, 'foo'), 'js land new[foo]', ':INTERNAL with new');

ok($instance.item === $instance, 'item');

is($instance.item(:INTERNAL, 'foo'), 'js land item[foo]', ':INTERNAL with item');
is($instance.sink(:INTERNAL, 'foo'), 'js land sink[foo]', ':INTERNAL with sink');
is($instance.Bool(:INTERNAL, 'foo'), 'js land Bool[foo]', ':INTERNAL with Bool');
is($instance.defined(:INTERNAL, 'foo'), 'js land defined[foo]', ':INTERNAL with defined');

my $check = EVAL(:lang<JavaScript>, q:to/END/);
  class TestArgs {
      isNull(arg) {
        return arg === null;
      }

      typeof(arg) {
        return typeof arg;
      }

      toString(arg) {
        return arg.toString();
      }
  }

  return new TestArgs();
END

is($check.typeof('Hello World'), 'string', 'right typeof of Str when passing to js');
is($check.typeof(123e0), 'number', 'right typeof of Num when passing to js');
is($check.typeof(True), 'boolean', 'right typeof of True when passing to js');
is($check.typeof(False), 'boolean', 'right typeof of False when passing to js');
is($check.typeof(123), 'bigint', 'right typeof of Int when passing to js');

is($check.toString('Hello World'), 'Hello World', 'right value of Str when passing to js');
is($check.toString(123e0), '123', 'right value of Num when passing to js');
is($check.toString(True), 'true', 'right value of True when passing to js');
is($check.toString(False), 'false', 'right value of False when passing to js');
is($check.toString(123), '123', 'right value of Int when passing to js');

ok($check.isNull(Mu), 'Mu gets turned into null');

# vim: expandtab shiftwidth=4
