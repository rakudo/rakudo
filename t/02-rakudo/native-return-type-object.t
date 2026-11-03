use Test;
use nqp;

plan :skip-all('native return coercion of a returned value is a RakuAST frontend behavior')
    unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
plan 44;

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
throws-like { return-str(1) }, X::TypeCheck::Return, message => /'expected int but got Str ("7")'/,
    'a native int return fails the return type check on a returned Str';

sub return-int(int $a --> int) { return $a + 1 }
is return-int(1), 2, 'a native int return hands back a returned value';

sub fail-int(int $a --> int) { fail "custom failure" }
is fail-int(1) // 'default', 'default',
    'a native int return hands back the Failure the routine fails with, still soft';
my $failure = fail-int(1);
nok $failure.handled, 'a native int return leaves the Failure it hands back unhandled';
isa-ok $failure, Failure, 'a native int return hands back the Failure itself';
is $failure.exception.message, 'custom failure',
    'a native int return hands back the Failure with its message';
sub failure-new(--> int) { Failure.new("custom failure") }
isa-ok failure-new(), Failure, 'a native int return hands back a Failure the routine falls off the end with';

sub num-int(--> num) { 3 }
throws-like { num-int() }, X::TypeCheck::Return, message => /'expected num but got Int (3)'/,
    'a native num return fails the return type check on an Int the routine falls off the end with';

sub str-int(--> str) { 42 }
throws-like { str-int() }, X::TypeCheck::Return, message => /'expected str but got Int (42)'/,
    'a native str return fails the return type check on an Int the routine falls off the end with';

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
throws-like { C.m(1) }, X::TypeCheck::Return, message => /'expected int but got Str ("7")'/,
    'a native int return fails the return type check on a Str a method falls off the end with';

my class R { method m(--> int32) { return self } }
throws-like { R.new.m }, X::TypeCheck::Return, message => /'expected int32 but got R'/,
    'a native int32 return fails the return type check on a returned object';

my class S is repr('CStruct') { has int32 $.a; method m(--> int32) { self } }
throws-like { S.new.m }, X::TypeCheck::Return, message => /'expected int32 but got S'/,
    'a native int32 return fails the return type check on a CStruct a method falls off the end with';

sub type-object-int(--> int) { Int }
throws-like { type-object-int() }, X::TypeCheck::Return, message => /'expected int but got Int (Int)'/,
    'a native int return fails the return type check on an Int type object';

my class Boxed { has int $.v is box_target }
sub boxed-int(--> int) { Boxed.new(v => 5) }
is boxed-int(), 5, 'a native int return unboxes an object with an int box target';

my class P is repr('CPointer') { }
my $pointer := nqp::box_i(6, P);
sub pointer-int(--> int) { return $pointer }
is pointer-int(), 6, 'a native int return unboxes a returned CPointer';

my class A { has $.h; method handle(--> int32) { $!h } }
throws-like { A.new.handle }, X::TypeCheck::Return, message => /'expected int32 but got Any (Any)'/,
    'a native int32 return fails the return type check on an attribute holding Any';

sub poly(Mu \x --> int) { x }
is-deeply poly(Nil), Nil, 'a native int return hands back a Nil at a call site that sees several types';
throws-like { poly("7") }, X::TypeCheck::Return, message => /'expected int but got Str ("7")'/,
    'a native int return fails the return type check at a call site that saw a Nil';
is poly(Boxed.new(v => 5)), 5, 'a native int return unboxes a box target object at a call site that saw other types';
is-deeply poly(Nil), Nil, 'a native int return hands back a Nil again at a call site that saw other types';

sub poly-return(Mu \x --> int) { return x }
is-deeply poly-return(Nil), Nil, 'a native int return hands back a returned Nil at a call site that sees several types';
throws-like { poly-return("7") }, X::TypeCheck::Return, message => /'expected int but got Str ("7")'/,
    'a native int return fails the return type check on a returned Str at a call site that saw a Nil';
is poly-return(Boxed.new(v => 5)), 5, 'a native int return unboxes a returned box target object at a call site that saw other types';

sub uint-str(--> uint) { "7" }
throws-like { uint-str() }, X::TypeCheck::Return, message => /'expected uint but got Str ("7")'/,
    'a native uint return fails the return type check on a Str';
sub uint-nil(--> uint) { Nil }
is-deeply uint-nil(), Nil, 'a native uint return hands back a Nil';
sub uint-pass(UInt $x --> uint) { $x }
is uint-pass(7), 7, 'a native uint return hands back an Int';
my class BoxedU { has uint $.v is box_target }
sub uint-boxed(--> uint) { BoxedU.new(v => 5) }
is uint-boxed(), 5, 'a native uint return unboxes an object with a uint box target';

sub num-pass(Num $x --> num) { $x }
is num-pass(1.5e0), 1.5e0, 'a native num return hands back a Num';
sub str-pass(Str $x --> str) { $x }
is str-pass("a"), "a", 'a native str return hands back a Str';
my class BoxedN { has num $.v is box_target }
sub num-boxed(--> num) { BoxedN.new(v => 2.5e0) }
is num-boxed(), 2.5e0, 'a native num return unboxes an object with a num box target';
my class BoxedS { has str $.v is box_target }
sub str-boxed(--> str) { BoxedS.new(v => "b") }
is str-boxed(), "b", 'a native str return unboxes an object with a str box target';
sub num-str(Str $x --> num) { $x }
throws-like { num-str("x") }, X::TypeCheck::Return, message => /'expected num but got Str ("x")'/,
    'a native num return fails the return type check on a Str';
sub str-num(Num $x --> str) { $x }
throws-like { str-num(1.5e0) }, X::TypeCheck::Return, message => /'expected str but got Num (1.5e0)'/,
    'a native str return fails the return type check on a Num';

# vim: expandtab shiftwidth=4
