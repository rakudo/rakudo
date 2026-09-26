use Test;
use nqp;

plan :skip-all('native return coercion of a returned value is a RakuAST frontend behavior')
    unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
plan 17;

# A routine with a native return type coerces the value it ends with,
# whether it falls off the end with it or returns it, and whether the
# call is inlined, made through a variable, or made as an operator on
# native operands. A Nil or a Failure passes as it does through any
# other return type.
sub nil-int(int $a --> int) { Nil }
sub nil-num(int $a --> num) { Nil }
sub nil-str(int $a --> str) { Nil }
sub add-one(int $a --> int) { $a + 1 }

is-deeply nil-int(1), Nil, 'a native int return hands back a Nil the routine falls off the end with';
is-deeply nil-num(1), Nil, 'a native num return hands back a Nil the routine falls off the end with';
is-deeply nil-str(1), Nil, 'a native str return hands back a Nil the routine falls off the end with';

my &via = &nil-int;
is-deeply via(1), Nil, 'a native int return hands back a Nil through a call by variable';

sub infix:<nil>(int $a, int $b --> int) { Nil }
my int $i = 1;
is-deeply ($i nil 1), Nil, 'a native int return hands back a Nil from an operator with native operands';

is add-one(1), 2, 'a native int return still hands back its value';

sub return-nil(int $a --> int) { return Nil }
is-deeply return-nil(1), Nil, 'a native int return hands back a returned Nil';

sub return-str(int $a --> int) { return "7" }
throws-like { return-str(1) }, Exception, message => /'cannot unbox to a native integer'/,
    'a native int return dies on a returned Str';

sub return-int(int $a --> int) { return $a + 1 }
is return-int(1), 2, 'a native int return hands back a returned value';

sub fail-int(int $a --> int) { fail "custom failure" }
is fail-int(1) // 'default', 'default',
    'a native int return hands back the Failure the routine fails with, still soft';

sub num-int(--> num) { 3 }
throws-like { num-int() }, Exception, message => /'cannot unbox to a native number'/,
    'a native num return dies on an Int the routine falls off the end with';

sub str-int(--> str) { 42 }
throws-like { str-int() }, Exception, message => /'cannot unbox to a native string'/,
    'a native str return dies on an Int the routine falls off the end with';

sub bool-int(--> int) { 1 == 1 }
is bool-int(), 1, 'a native int return coerces a Bool the routine falls off the end with';

sub bump(int $a is rw --> int) { $a = $a + 1 }
my int $b = 1;
is bump($b), 2, 'a native int return hands back the value a native assignment stores';

my int $slot = 1;
sub read-slot() returns int { $slot }
dies-ok { read-slot() = 2 }, 'a native int return of a native variable read is not an lvalue';
is $slot, 1, 'the native variable behind a native int return is left alone';

my class C { method m(int $a --> int) { "7" } }
throws-like { C.m(1) }, Exception, message => /'cannot unbox to a native integer'/,
    'a native int return dies on a Str a method falls off the end with';

# vim: expandtab shiftwidth=4
