use Test;

plan 5;

# A `sub foo` declaration is hoisted to the start of its enclosing
# lexical scope, so a call earlier in the same scope must resolve to
# the local declaration rather than to an outer (typically `CORE`)
# routine of the same name.

sub forward-shadows-core() {
    my @x; my @y;
    my $r = reduce(@x, @y);
    sub reduce(@a, @b) { 'local' }
    $r
}
is forward-shadows-core(), 'local',
    'a forward call resolves to a same-scope `sub` shadowing CORE';

sub forward-without-shadow() {
    my $r = inner();
    sub inner { 'inner' }
    $r
}
is forward-without-shadow(), 'inner',
    'a forward call resolves to a same-scope `sub` with no CORE name in play';

is reduce(&[+], 1, 2, 3), 6,
    'a sub-form call with no same-scope `sub` shadow still resolves through CORE';

sub forward-multi-shadows-core() {
    my $r = process(42);
    multi sub process(Int $x) { "int: $x" }
    multi sub process(Str $x) { "str: $x" }
    $r
}
is forward-multi-shadows-core(), 'int: 42',
    'a forward call resolves to a same-scope `multi sub`';

sub forward-our-sub() {
    my $r = ours();
    our sub ours { 'ours' }
    $r
}
is forward-our-sub(), 'ours',
    'a forward call resolves to a same-scope `our sub`';

# vim: expandtab shiftwidth=4
