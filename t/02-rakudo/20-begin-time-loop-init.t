use Test;

# `while`/`until`/`repeat-while`/`repeat-until` used as expressions emit
# through `Seq.from-loop`. When such a loop appears inside a
# `constant X = do { ... }` initializer that runs at BEGIN, the body
# must still execute for its side effects, and a `repeat` form must
# run the body once before the condition is checked. A constant whose
# final expression is itself the accumulated result also has to keep
# working - the early sink propagation must not strip the result of
# the do-block.

constant WHILE-COUNT = do {
    my $i = 0;
    while $i < 3 {
        $i++;
    }
    $i
};
is WHILE-COUNT, 3,
  'while body inside a BEGIN-time initializer runs for side effects';

constant UNTIL-COUNT = do {
    my $i = 0;
    until $i >= 3 {
        $i++;
    }
    $i
};
is UNTIL-COUNT, 3,
  'until body inside a BEGIN-time initializer runs for side effects';

constant REPEAT-WHILE-COUNT = do {
    my $i = 0;
    my @log;
    repeat {
        @log.push($i);
    } while ++$i < 3;
    @log.elems
};
is REPEAT-WHILE-COUNT, 3,
  'repeat-while body inside a BEGIN-time initializer runs before checking the condition';

constant REPEAT-UNTIL-COUNT = do {
    my $i = 0;
    my @log;
    repeat {
        @log.push($i);
    } until ++$i >= 3;
    @log.elems
};
is REPEAT-UNTIL-COUNT, 3,
  'repeat-until body inside a BEGIN-time initializer runs before checking the condition';

constant ACCUMULATED = do {
    my @a;
    my $i = 0;
    while $i < 3 {
        @a.push: $i;
        $i++;
    }
    @a
};
is ACCUMULATED.elems, 3,
  'a constant whose value is the side-effect result of a preceding loop is kept';
is ACCUMULATED[2], 2,
  'and the accumulated value reflects every iteration of that loop';

done-testing;

# vim: expandtab shiftwidth=4
