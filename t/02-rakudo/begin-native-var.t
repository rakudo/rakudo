use nqp;
use Test;

# A native-typed scalar used in BEGIN-time code refers to a slot in the
# declaring frame, which does not exist at BEGIN time and has no container
# the BEGIN-time code could share. The dynamically compiled BEGIN code
# declares a fresh slot of the declared native type instead, so reads see
# the type default and a write stays local to the BEGIN. The legacy
# frontend does not compile these forms, so they are skipped there.

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 10;

if $rakuast {
    lives-ok { EVAL 'BEGIN my int $foo' },
      'a BEGIN whose statement declares a native int compiles';

    is EVAL('BEGIN my int $foo = 5'), 5,
      'a BEGIN produces the value assigned to a native int it declares';

    is EVAL('BEGIN my num $foo = 2.5e0'), 2.5e0,
      'a BEGIN produces the value assigned to a native num it declares';

    is EVAL('BEGIN my str $foo = "hi"'), 'hi',
      'a BEGIN produces the value assigned to a native str it declares';

    is EVAL('my int $foo; my $x = BEGIN $foo; $x'), 0,
      'a BEGIN reading an outer native int sees the type default';

    is EVAL('my int $foo; BEGIN $foo = 5; $foo'), 0,
      'a BEGIN-time write to an outer native int stays local to the BEGIN';

    is-deeply EVAL('BEGIN my uint $foo = 7'), 7,
      'a BEGIN produces the boxed value assigned to a native uint it declares';

    is EVAL('my int $x; constant y = $x + 1; y'), 1,
      'a constant initializer reading a native int sees the type default';

    is EVAL('my str $x; constant y = $x ~ "!"; y'), '!',
      'a constant initializer reading a native str sees the type default';
}
else {
    skip 'native scalars in BEGIN-time code are NYI on the legacy frontend', 9;
}

# Native aggregates have containers, which BEGIN-time code shares.
is do { my int @a; BEGIN @a.push(3); @a[0] }, 3,
  'a BEGIN-time push to a native int array is visible at runtime';

# vim: expandtab shiftwidth=4
