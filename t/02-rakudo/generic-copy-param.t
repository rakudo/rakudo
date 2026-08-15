use Test;

plan 23;

# An `is copy` parameter wraps its argument in a fresh Scalar whose container
# descriptor comes from the Parameter object. For a parameter with a generic
# type the descriptor holds the unresolved type variable until it is
# instantiated on each call from the frame where the type variable is bound.
# These tests exercise that instantiation across the binding paths.

role IntShape[::T] {
    method bump(T $x is copy) { $x = $x + 1; $x }
}
is IntShape[Int].new.bump(5), 6,
    'a role method can assign to a generic copy param via the pun';

role StrShape[::T] {
    method shout(T $x is copy) { $x ~= '!'; $x }
}
class Shouter does StrShape[Str] { }
is Shouter.new.shout('a'), 'a!',
    'a role method composed into a class can assign to a generic copy param';

# One role instantiated at two types must enforce each type separately.
role Settable[::T] {
    method set(T $x is copy, $v) { $x = $v; $x }
}
is Settable[Int].new.set(1, 2), 2,
    'the Int instantiation of a role accepts an Int assignment';
is Settable[Str].new.set('a', 'b'), 'b',
    'the Str instantiation of the same role accepts a Str assignment';
throws-like { Settable[Str].new.set('a', 5) }, X::TypeCheck::Assignment,
    'the Str instantiation of the same role rejects an Int assignment';

sub double(::T $a, T $x is copy) { $x = $x * 2; $x }
is double(Int, 5), 10,
    'a sub can assign to a copy param typed by a capture in the same signature';

sub bump-definite(::T $a, T:D $x is copy) { $x = $x + 1; $x }
is bump-definite(Int, 41), 42,
    'a T:D copy param accepts assignment after instantiation';

sub coerce(::T $a, T() $x is copy) { $x = $x + 1; $x }
is coerce(Int, '41'), 42,
    'a coercive generic copy param coerces its argument and accepts assignment';

role Chainable {
    method chain(::?CLASS $x is copy) { $x = self; $x }
}
class Chained does Chainable { }
isa-ok Chained.new.chain(Chained), Chained,
    'a ::?CLASS copy param accepts assignment in a composing class';

sub bump-named(::T $a, T :$x is copy) { $x = $x + 1; $x }
is bump-named(Int, x => 5), 6,
    'a named generic copy param accepts assignment';

sub bump-default(::T $a, T $x is copy = 10) { $x = $x + 1; $x }
is bump-default(Int), 11,
    'an omitted generic copy param assigns over its default';
is bump-default(Int, 20), 21,
    'a passed generic copy param with a default accepts assignment';

multi bump-multi(::T $a, T $x is copy) { $x = $x + 1; $x }
is bump-multi(Int, 5), 6,
    'a multi sub can assign to a generic copy param';

role MultiShape[::T] {
    multi method bump(T $x is copy) { $x = $x + 1; $x }
}
is MultiShape[Int].new.bump(5), 6,
    'a role multi method can assign to a generic copy param';

# Sub-signature parameter, which binds through the runtime binder rather
# than the lowered signature code.
sub bump-subsig(::T $a, @b ($x, T $y is copy)) { $y = $y + 1; $y }
is bump-subsig(Int, [1, 5]), 6,
    'a generic copy param inside a sub-signature accepts assignment';

role Strict[::T] {
    method poke(T $x is copy) { $x = 'oops'; $x }
}
throws-like { Strict[Int].new.poke(5) }, X::TypeCheck::Assignment,
    'the instantiated copy container still rejects a wrongly typed assignment';

sub report-of(::T $a, T $x is copy) { $x.VAR.of }
ok report-of(Int, 5) =:= Int,
    'the copy container of a generic copy param reports the instantiated type';

sub renil(::T $a, T $x is copy) { $x = Nil; $x }
ok renil(Int, 5) =:= Int,
    'assigning Nil to a generic copy param restores the instantiated default';

sub keep(::T $a, T $x is copy) { $x = $x + 1 }
my $orig = 5;
keep(Int, $orig);
is $orig, 5,
    'assigning to a generic copy param leaves the argument untouched';

sub pick-pos(::T $a, T $x is copy where * > 0) { $x = $x + 1; $x }
is pick-pos(Int, 5), 6,
    'a constrained generic copy param accepts assignment';

sub plain(Int $x is copy) { $x = $x + 1; $x }
is plain(5), 6,
    'a non-generic typed copy param still accepts assignment';
my $bad = 'nope';
dies-ok { plain($bad) },
    'a non-generic typed copy param still rejects a wrongly typed argument';
sub loose($x is copy) { $x = 'any'; $x }
is loose(5), 'any',
    'an untyped copy param still accepts any assignment';

# vim: expandtab shiftwidth=4
