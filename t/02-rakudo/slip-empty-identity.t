use Test;
use nqp;

# The SlippyIterator role recognizes Empty by identity so that a block
# returning it contributes nothing.  The iterators these cases use reach
# slip-all when the result is consumed whole and start-slip when it is
# pulled one value at a time, so the cases below do both.

plan 31;

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

# The iterators a map or grep runs on skip the start-slip call for an
# Empty, so each of them pulls one value at a time across rejected values.
{
    my $it = (1..5).grep({ False }).iterator;
    ok $it.pull-one =:= IterationEnd,
      'pulling from a grep that rejects every value gives IterationEnd';
    ok $it.pull-one =:= IterationEnd,
      'pulling again after exhaustion still gives IterationEnd';
}
{
    my $it = (1..5).grep(* == 1).iterator;
    is $it.pull-one, 1,
      'pulling from a grep gives the accepted value';
    ok $it.pull-one =:= IterationEnd,
      'a grep that rejects the rest of the source ends with IterationEnd';
}
{
    my $held-empty = Empty;
    my $it = (1..3).map({ $_ == 3 ?? 9 !! $held-empty }).iterator;
    is $it.pull-one, 9,
      'an Empty held in a container is skipped when pulling one value at a time';
    ok $it.pull-one =:= IterationEnd,
      'and the source then ends with IterationEnd';
}
{
    my $it = (1..4).map({ $_ == 4 ?? slip(1, 2) !! Empty }).iterator;
    is ($it.pull-one, $it.pull-one).join(' '), '1 2',
      'a Slip after a run of Empty is pulled one value at a time';
    ok $it.pull-one =:= IterationEnd,
      'and IterationEnd follows the last value of the Slip';
}
{
    my $it = (1..10).grep({ next if $_ < 4; last if $_ > 7; $_ %% 2 }).iterator;
    my @r;
    until (my $v := $it.pull-one) =:= IterationEnd { @r.push: $v }
    is @r.join(' '), '4 6',
      'next and last work between rejected values when pulling one value at a time';
}
{
    my $it = (1..6).map(-> $a, $b { $a == 5 ?? $a + $b !! Empty }).iterator;
    is $it.pull-one, 11,
      'a two parameter block returning Empty is skipped when pulling one value at a time';
    ok $it.pull-one =:= IterationEnd,
      'and the two parameter map then ends with IterationEnd';
}
{
    my $it = (1..9).map(-> $a, $b, $c { $a == 7 ?? $a + $b + $c !! Empty }).iterator;
    is $it.pull-one, 24,
      'a three parameter block returning Empty is skipped when pulling one value at a time';
}
{
    my $seen = 0;
    my $it = (1..4).map({ LAST { $seen = 1 }; $_ == 4 ?? 8 !! Empty }).iterator;
    is $it.pull-one, 8,
      'a block with a phaser returning Empty is skipped when pulling one value at a time';
}

# vim: expandtab shiftwidth=4
