use lib <t/packages/02-rakudo/lib>;
use nqp;
use Test;

# A native-typed constant must present its native type at a call site, so a
# reference to it selects a native multi candidate the same way a native
# variable does. Previously the reference was compiled as a lookup of the
# boxed value, which only ever matched the boxed candidate. Selecting a
# native candidate this way is implemented only on the RakuAST frontend.

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 8;

{
    my int constant PICK = 0;
    multi foo(int) { 'native' }
    if $rakuast {
        is foo(PICK), 'native',
          'native int constant selects an int multi candidate';
    }
    else {
        skip 'native-typed constant dispatch is NYI on the legacy frontend';
    }
}

{
    my int constant PICK = 5;
    multi bar(int) { 'native' }
    multi bar(Int) { 'boxed'  }
    if $rakuast {
        is bar(PICK), 'native',
          'native int constant prefers the int candidate over the Int candidate';
    }
    else {
        skip 'native-typed constant dispatch is NYI on the legacy frontend';
    }
}

{
    my num constant N = 1.5e0;
    multi baz(num) { 'native' }
    if $rakuast {
        is baz(N), 'native',
          'native num constant selects a num multi candidate';
    }
    else {
        skip 'native-typed constant dispatch is NYI on the legacy frontend';
    }
}

{
    my str constant S = 'hi';
    multi qux(str) { 'native' }
    if $rakuast {
        is qux(S), 'native',
          'native str constant selects a str multi candidate';
    }
    else {
        skip 'native-typed constant dispatch is NYI on the legacy frontend';
    }
}

# The value and its boxed behaviour are unchanged in object context.
{
    my int constant I = 42;
    is I, 42, 'a native int constant reads back its value';
    is I.^name, 'Int', 'a native int constant boxes to Int in object context';
    is I + 8, 50, 'a native int constant works in arithmetic';
}

# A non-native constant is unaffected and still matches its boxed candidate.
{
    constant C = 10;
    multi quux(Int) { 'boxed' }
    is quux(C), 'boxed',
      'a non-native constant still selects the boxed candidate';
}

# vim: expandtab shiftwidth=4
