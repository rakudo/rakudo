use Test;
use nqp;

# A method is added to its class as soon as it is declared, so BEGIN time
# code later in the class body can call it before the class is composed.
# Methods the class only gets at compose time give a message saying so.
# Messages are word wrapped, so they are compared with whitespace collapsed.

plan 22;

is EVAL(q[
    class Golf {
        method a { 42 }
        method b { BEGIN Golf.a }
    }
    Golf.b
]), 42, 'a BEGIN later in the class body can call an earlier method';

is EVAL(q[
    class WithConstant {
        method a { 42 }
        constant X = WithConstant.a;
        method b { X }
    }
    WithConstant.b
]), 42, 'a constant later in the class body can call an earlier method';

is-deeply EVAL(q[
    class Introspected {
        method a { }
        method b { }
        constant N = Introspected.^methods(:local).map(*.name).List;
        method c { }
        method n { N }
    }
    Introspected.n
]), <a b>, 'introspection before compose lists the methods declared so far';

is EVAL(q[
    class WithPrivate {
        method !p { 42 }
        constant P = WithPrivate.^find_private_method('p')(WithPrivate);
        method n { P }
    }
    WithPrivate.n
]), 42, 'a private method is in the private method table before compose';

is EVAL(q[
    class WithSubmethod {
        submethod s { 42 }
        constant S = WithSubmethod.s;
        method n { S }
    }
    WithSubmethod.n
]), 42, 'a submethod is callable before compose';

is-deeply EVAL(q[
    grammar WithToken {
        token a { x }
        constant N = WithToken.^methods(:local).map(*.name).List;
        method n { N }
    }
    WithToken.n
]), ('a',), 'a token is in its grammar before compose';

is EVAL(q[
    class Stubbed { ... }
    class Stubbed {
        method a { 42 }
        constant X = Stubbed.a;
        method b { X }
    }
    Stubbed.b
]), 42, 'a method of a class that replaces a stub is callable before compose';

is-deeply EVAL(q[
    multi trait_mod:<is>(Method:D $m, :$seen!) {
        my @names = $m.package.^methods(:local).map(*.name);
        $m.package.^add_method('seen', my method () { @names.List });
    }
    class Traited {
        method a { }
        method b is seen { }
    }
    Traited.seen
]), ('a',), 'a method trait sees the earlier methods but not its own method';

is-deeply EVAL(q[
    multi trait_mod:<is>(Method:D $m, :$seen-in-role!) {
        my @names = $m.package.^methods(:local).map(*.name);
        $m.package.^add_method('seen', my method () { @names.List });
    }
    role TraitedRole {
        method a { }
        method b is seen-in-role { }
    }
    class DoesTraitedRole does TraitedRole { }
    DoesTraitedRole.seen
]), ('a',), 'a method trait in a role sees the earlier methods of the role';

throws-like q[class DupMethod {
    method a { }
    method a { }
}], X::Method::Duplicate, line => 3,
  'a duplicate method is reported at the second declaration';

throws-like q[class DupPrivate {
    method !a { }
    method !a { }
}], X::Comp, line => 3, message => /'already declared'/,
  'a duplicate private method is reported at the second declaration';

throws-like q[use MONKEY-TYPING; augment class Int {
    method Str { }
}], X::Method::Duplicate, line => 2,
  'a method that augment redeclares is reported at the method';

throws-like q[class Twice { method a { }; method a { } }],
  X::Method::Duplicate,
  'a class with a duplicate method fails to compile';

throws-like q[class Twice { method a { }; method a { } }],
  X::Method::Duplicate,
  'declaring the same failing class again reports the same error';

class Kept { method k { 1 } }
try EVAL q[
    use MONKEY-TYPING;
    augment class Kept { method a { }; method a { } }
];
is Kept.k, 1, 'a failed augment leaves the augmented class in place';

# The legacy frontend reuses the uncomposed type a failed declaration
# leaves behind, together with the members it already got.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[
        multi trait_mod:<is>(Method:D $m, :$dies!) { die 'trait died' }
        try EVAL 'class Retried { method m { 1 }; method n is dies { } }';
        EVAL 'class Retried { method m { 1 }; method n { 2 } }; Retried.m + Retried.n'
    ]), 3, 'a corrected retry of a class whose later method trait died compiles';

    is EVAL(q[
        multi trait_mod:<is>(Method:D $m, :$dies!) { die 'trait died' }
        try EVAL 'class RetriedAttribute { has $.x; method n is dies { } }';
        EVAL 'class RetriedAttribute { has $.x; method n { 2 } }; RetriedAttribute.n'
    ]), 2, 'a corrected retry of a class with an attribute compiles';
}
else {
    skip 'the legacy frontend reuses the type of a failed declaration', 2;
}

throws-like q[class MultiCall { multi method a { 42 }; BEGIN MultiCall.a }],
  X::Comp::BeginTime,
  exception => {
      .message.words.join(' ') ~~ /
          ^ "No such method 'a' for invocant of type 'MultiCall'."
          .* "'MultiCall' is not composed yet"
      /
  },
  'calling a multi method before compose says the class is not composed yet';

throws-like q[class Inherits { BEGIN Inherits.new }],
  X::Comp::BeginTime,
  exception => {
      .message.words.join(' ').contains("'Inherits' is not composed yet")
  },
  'calling a method of the default parent before compose says the class is not composed yet';

throws-like q[
    class Parent { method parent-meth { } }
    class Kid is Parent { BEGIN Kid.parent-mehh }
], X::Comp::BeginTime,
  exception => {
      .message.words.join(' ') ~~ /
          "'Kid' is not composed yet" .* "Did you mean 'parent-meth'?"
      /
  },
  'a class with a parent keeps its suggestions before compose';

throws-like { Metamodel::ClassHOW.new_type(:name<MopMade>).nope },
  X::Method::NotFound,
  message => { .words.join(' ').contains("'MopMade' is not composed yet") },
  'a missing method on an uncomposed MOP class says it is not composed yet';

throws-like q[class Composed { }; Composed.nope],
  X::Method::NotFound,
  message => { .contains("No such method 'nope'") && !.contains('composed') },
  'a missing method on a composed class does not mention composing';

# vim: expandtab shiftwidth=4
