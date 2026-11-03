use Test;

plan 6;

sub returns-failure { fail "boom" }

# A bare block does not decontainerize its return value the way a routine does.
# So when a postfix `()` call of a block is sunk, its result is decontainerized
# here, otherwise a returned Scalar holding a Failure stays a container and
# sinking a container is a no-op. Math::Angle relies on this via
# `dies-ok { $failure }`.
my $f = returns-failure();
dies-ok { $f }, 'a Failure reached through a sunk postfix call throws';

my $g = returns-failure();
my &blk = { $g };
dies-ok { blk() }, 'sinking a postfix call returning a Failure throws';

# A routine decontainerizes its own return, and an `is rw` one deliberately
# keeps its container. So a sunk postfix call of a routine is left alone: an
# is-rw routine's returned container must not be sunk.
my $rw-sunk = False;
my $rw = sub () is rw { my $o = class { method sink { $rw-sunk = True } }.new; $o };
$rw();
nok $rw-sunk, 'sinking an is-rw routine call result does not sink its container';

# Likewise for a named is-rw routine (the roast S04 sink.t spec).
my $sunk = False;
my sub wrap is rw { my $obj = class { method sink { $sunk = True } }.new; $obj }
wrap();
nok $sunk, 'sinking an is-rw container return does not sink its contents';

# Binding a block call result (not sinking) preserves its container.
my $c = { my $x = 42; $x };
my $r := $c();
ok $r.VAR ~~ Scalar, 'binding a block call result preserves its container';

# A non-Failure postfix call result sunk is fine.
lives-ok { my &b = { 42 }; b() }, 'sinking a non-Failure postfix call result lives';

# vim: expandtab shiftwidth=4
