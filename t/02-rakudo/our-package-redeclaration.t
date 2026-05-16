use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 7;

is-run q:to/CODE/,
    EVAL q[class A {}];
    EVAL q[class A {}];
    print "no-error";
    CODE
    :err(/'Redeclaration of symbol' .* "'A'"/),
    :exitcode(1), :out(''),
    'cross-EVAL identifier class is a Redeclaration';

is-run q:to/CODE/,
    EVAL q[class A::B {}];
    EVAL q[class A::B {}];
    print "no-error";
    CODE
    :err(/'Redeclaration of symbol' .* "'A::B'"/),
    :exitcode(1), :out(''),
    'cross-EVAL multi-part class is a Redeclaration';

is-run q:to/CODE/,
    class A::B {};
    class A::B {};
    print "no-error";
    CODE
    :err(/'Redeclaration of symbol' .* "'A::B'"/),
    :exitcode(1), :out(''),
    'in-source multi-part dupe is a Redeclaration';

is-run q:to/CODE/,
    class Foo { ... };
    class Foo { method bar { 42 } };
    print Foo.new.bar;
    CODE
    :out('42'), :err(''),
    'stub-then-real composes and runs';

is-run q:to/CODE/,
    { my grammar G { token TOP { . } }; class G::A {} };
    { my grammar G { token TOP { . } }; class G::A {} };
    print "ok";
    CODE
    :out('ok'), :err(''),
    'sibling my-scoped parents do not collide';

is-run q:to/CODE/,
    class AAAA { class B { } };
    use MONKEY-TYPING;
    augment class AAAA { class B { } };
    print "no-error";
    CODE
    :err(/'Redeclaration of symbol' .* "'B'"/),
    :exitcode(1), :out(''),
    'augment-body nested class redecl is a Redeclaration';

is-run q:to/CODE/,
    my $a = $*TMPDIR.add("rakuast-reload-a-{$*PID}.rakumod");
    my $b = $*TMPDIR.add("rakuast-reload-b-{$*PID}.rakumod");
    $a.spurt(q[class CompUnit::Repository::Staging { method new(|) { self } }]);
    $b.spurt(q[class CompUnit::Repository::Staging { method new(|) { self } }]);
    LEAVE { $a.unlink if $a.e; $b.unlink if $b.e; }
    $*REPO.load($a);
    $*REPO.load($b);
    print "ok";
    CODE
    :out('ok'), :err(''),
    'cross-compunit $*REPO.load reload silent-replaces';

# vim: expandtab shiftwidth=4
