use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 7;

# A try block runs with `use fatal` semantics: a Failure produced inside it
# is thrown and caught, so a `return` of a failing expression never happens
# and control falls through after the try.
is do {
    sub f($v) { try { return $v.Int }; 'fell-through' }
    f("abc")
}, 'fell-through', 'a Failure as a return argument in a try is caught, not returned';

# A successful conversion still returns its value.
is do {
    sub g($v) { try { return $v.Int }; 'fell-through' }
    g("42")
}, 42, 'a successful return in a try returns its value';

# A Failure assigned and then returned is likewise caught.
is do {
    sub h($v) { try { my $n = $v.Int; return $n }; 0 }
    h("xyz")
}, 0, 'a Failure bound then returned in a try is caught';

# An explicit `no fatal` in the block is honored: the try does not fatalize,
# so a returned Failure stays soft rather than being caught.
is do {
    sub k($v) { try { no fatal; return $v.Int }; 'fell-through' }
    k("abc").WHAT.^name
}, 'Failure', 'no fatal in a try block leaves a returned Failure soft';

# Under `no fatal` a soft Failure that is the block's value is kept, rather
# than the try sinking it away.
is do {
    my $r = try { no fatal; "abc".Int };
    $r.WHAT.^name
}, 'Failure', 'no fatal in a try keeps a soft Failure result value';

# Fatalizing the block is the runtime half of `use fatal`. It must not turn the
# block's compile-time worries into sorries: a sunk statement that merely warns
# still compiles and runs inside a try.
is-run 'try { 42; 1 }; print "compiled"',
    :out("compiled"),
    :err(/'Useless use of constant integer 42 in sink context'/),
    'a worry inside a try stays a warning, the block still compiles';

# Explicit `use fatal` does turn a worry into a compile error, so the runtime
# and compile-time halves stay separate.
is-run 'use fatal; |4..5',
    :exitcode(* != 0),
    :err(/'parenthesize'/),
    'use fatal still promotes the worry to a compile error';

# vim: expandtab shiftwidth=4
