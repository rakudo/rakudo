use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

# Properties the specializations in Rakudo::Sorting rest on, neither of
# which shows up in the value a sort returns, so neither is reachable by
# asserting what a sort produces.

plan 5;

# --- what the setting's scope gains ------------------------------------
# A sub written beside a class rather than inside it becomes a name every
# Raku program can see and none can use.

sub visible($name) { (try EVAL("\&$name")).defined || (try EVAL($name)).defined }

ok visible('ORDER'),
  'a sub the setting means to publish is reachable, so this check can fail';
nok visible('slot-reads-the-same'),
  'the sort helper stays out of the scope every program compiles in';
nok visible('SORT-CLASSIFY-FROM'),
  'the length the sort classifies from stays out of it too';

# --- what a refused container costs ------------------------------------
# The classifier refuses a container that answers with code of its own,
# and has to reach that verdict without running that code.  Reading the
# slot to decide, or binding it through a parameter that reads it, adds
# a fetch per element to the very containers being kept out.

sub fetches-sorting(int $n) {
    my $reads = 0;
    my @list;
    for ^$n -> $i {
        if $i == 3 {
            @list[$i] := Proxy.new(
              FETCH => method () { $reads++; 99 }, STORE => method ($x) {});
        }
        else {
            @list[$i] = $i;
        }
    }
    @list.sort.List;
    $reads
}

is fetches-sorting(15), 25,
  'a Proxy in a list too short to classify is read only by the merge';
is fetches-sorting(20), 35,
  'a Proxy in a list long enough to classify is read no more often';

# vim: expandtab shiftwidth=4
