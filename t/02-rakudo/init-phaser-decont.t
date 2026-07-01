use Test;

plan 8;

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

# A block that bound no value (it read a variable not yet bound when INIT ran)
# yields Mu, not a raw VMNull that dies on use.
my $src = 42;
my $alias := $src;
my $unbound = INIT $alias;
is $unbound.raku, 'Mu', 'INIT of a not-yet-bound variable yields Mu, not a VMNull';

# A constant initialized from INIT is evaluated at BEGIN, before INIT runs, so
# it is Mu rather than the phaser block itself.
my $const = EVAL 'constant K = INIT 42; K';
is $const.raku, 'Mu', 'a constant from INIT is Mu at BEGIN, not the block';

# vim: expandtab shiftwidth=4
