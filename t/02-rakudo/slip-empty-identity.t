use Test;
use nqp;

# The SlippyIterator role recognizes Empty by identity so that a block
# returning it contributes nothing.  The iterators these cases use reach
# slip-all when the result is consumed whole and start-slip when it is
# pulled one value at a time, so the cases below do both.

plan 18;

is-deeply (1..4).map({ $_ %% 2 ?? Empty !! $_ }).List, (1,3),
  'a block returning Empty contributes no element';
is-deeply (1..3).map({ slip($_, $_) }).List, (1,1,2,2,3,3),
  'a block returning a Slip contributes each of its elements';
is-deeply (1..3).map({ $_ == 2 ?? slip() !! $_ }).List, (1,3),
  'an empty Slip contributes no element';
is-deeply (1..3).map({ Empty }).List, (),
  'a block returning Empty for every element yields an empty list';
is-deeply ((1..3).map({ $_ == 2 ?? Slip !! $_ }).List), (1, Slip, 3),
  'the Slip type object is passed through as a value';
is-deeply (1..4).grep({ $_ %% 2 }).map({ slip($_, -$_) }).List, (2,-2,4,-4),
  'a Slip returned downstream of a grep still flattens';

is-deeply (1..100).map({ slip($_, $_) }).head(3).List, (1,1,2),
  'a Slip flattens when the result is pulled one value at a time';
is-deeply (1..Inf).map({ $_ %% 3 ?? slip($_, -$_) !! Empty }).head(4).List, (3,-3,6,-6),
  'Empty and Slip interleave when pulled from a lazy source';
is-deeply (1..3).map({ $_ == 2 ?? Slip !! $_ }).head(3).List, (1, Slip, 3),
  'the Slip type object is pulled through as a value';
my $sunk = 0;
sink (1..6).map({ $sunk++; $_ %% 2 ?? Empty !! $_ });
is $sunk, 6, 'sinking a mapped list still runs the block for every element';

# The Slip arrives from a variable rather than as a literal term, so it
# reaches the iterator inside a container.
my $held-empty = Empty;
is-deeply (1..3).map({ $_ == 2 ?? $held-empty !! $_ }).List, (1,3),
  'an Empty held in a container contributes no element';
my $held-slip = slip(7,8);
is-deeply (1..3).map({ $_ == 2 ?? $held-slip !! $_ }).List, (1,7,8,3),
  'a Slip held in a container contributes each of its elements';
my Slip $held-type;
is-deeply (1..3).map({ $_ == 2 ?? $held-type !! $_ }).List, (1, Slip, 3),
  'a Slip type object held in a container is passed through as a value';

# Taking the Slip out of the container rather than passing the container
# on is what keeps the source variable from staying aliased into the
# reified result.
my Slip $aliased;
my $reified := (1..3).map({ $_ == 2 ?? $aliased !! $_ }).List;
$reified.elems;
$aliased = slip(9);
is-deeply $reified, (1, Slip, 3),
  'a Slip type object taken from a container is not left aliased to it';

# --- what recognizing Empty leaves behind ------------------------------
# This one does not show up in any value a map or grep produces.
# start-slip returns early for Empty, and a slip already in progress is
# not its to discard.

my class Driver does Rakudo::SlippyIterator {
    method pull-one() { IterationEnd }
}
my $driver := Driver.new;
is $driver.start-slip(slip(1,2,3)), 1,
  'starting a slip yields its first value';
nok nqp::isnull(nqp::getattr($driver, Driver, '$!slipper')),
  'and leaves the rest of it queued';
is $driver.start-slip(Empty), IterationEnd,
  'starting an Empty yields IterationEnd';
my $rest := nqp::create(IterationBuffer);
$driver.push-rest($rest);
is nqp::elems($rest), 2,
  'and leaves the queued slip untouched';

# vim: expandtab shiftwidth=4
