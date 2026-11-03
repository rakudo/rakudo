use Test;
use nqp;

plan 5;

# A require merges the symbols of the unit it loads into the %?REQUIRE-SYMBOLS
# of its scope, which an indirect lookup consults first. Each call of the
# routine holding the require starts with an empty one, so a package merged
# by an earlier call cannot shadow the one a later call looks up.

sub required-symbols($name) {
    require ::($name);
    %?REQUIRE-SYMBOLS.keys.sort.List
}

is-deeply required-symbols('NativeCall::Types'), ('NativeCall',),
    'the first call holds the symbols of the unit it required';
is-deeply required-symbols('Test'), ('Test',),
    'a later call does not hold the symbols an earlier call required';

sub capture($name) {
    require ::($name);
    -> { %?REQUIRE-SYMBOLS.keys.sort.List }
}
my &first  = capture('NativeCall::Types');
my &second = capture('Test');
is-deeply first(), ('NativeCall',),
    'a closure keeps the symbols of the call that made it';

sub nested($name, $inner) {
    require ::($name);
    nested($inner, Nil) if $inner;
    %?REQUIRE-SYMBOLS.keys.sort.List
}
is-deeply nested('NativeCall::Types', 'Test'), ('NativeCall',),
    'a recursive call does not add to the symbols of its caller';

todo 'the legacy frontend starts each entry with an empty %?REQUIRE-SYMBOLS'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is EVAL(q[BEGIN require ::('NativeCall::Types'); ::('NativeCall::Types').^name]),
    'NativeCall::Types',
    'the symbols of a BEGIN time require remain at run time';

# vim: expandtab shiftwidth=4
