use Test;

plan 6;

# The value of an `INIT` statement prefix is the value the block produced, not
# the Scalar the phaser caches it in. `my $x = INIT $y` must see the same thing
# as `my $x = $y`, so a later method call reaches the value, not a container.

my $scalar = 42;
my $init = INIT $scalar;
isnt $init.WHAT.^name, 'Scalar', 'INIT of a container yields the value, not a container';
is $init, 42, 'the INIT value is correct';
is $init + 1, 43, 'the INIT value is usable as a value';

# The pattern from Test::Scheduler: a container captured through INIT and used
# as an invocant later.
class Widget { method poke { 'poked' } }
my $widget = Widget.new;
my $captured = INIT $widget;
is $captured.poke, 'poked', 'a method call reaches the value captured through INIT';

# A dynamic variable behaves the same.
my $*DYN = Widget.new;
my $from-dyn = INIT $*DYN;
is $from-dyn.poke, 'poked', 'INIT of a dynamic variable yields its value';

# The block form is likewise decontainerized.
my $c = 7;
my $block-init = INIT { $c };
isnt $block-init.WHAT.^name, 'Scalar', 'the INIT block form also yields the value';

# vim: expandtab shiftwidth=4
