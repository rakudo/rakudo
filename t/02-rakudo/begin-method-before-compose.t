use Test;
use nqp;

# A method is added to its class as soon as it is declared, so BEGIN time
# code later in the class body can call it before the class is composed.

plan 17;

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

# vim: expandtab shiftwidth=4
