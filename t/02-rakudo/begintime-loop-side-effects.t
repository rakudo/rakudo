use Test;

plan 4;

# A `my constant T = do { ... }` evaluates its initializer at BEGIN
# time. Inside the do-block, non-final statements are sunk and so a
# `while` / `until` / `loop` / `repeat` should run imperatively for
# its side effects. Without sink propagation through the BEGIN-time
# expression, the loop compiles to a lazy Seq that the constant
# initializer never iterates, and any list the body mutates is
# observed at its pre-loop state.

my constant WHILE-RESULT = do {
    my @arr;
    my $i = 0;
    while $i < 3 { @arr.push($i); $i = $i + 1 }
    @arr
};
is-deeply WHILE-RESULT.List, (0, 1, 2),
    'while body runs for side effects in a BEGIN-time do-block';

my constant UNTIL-RESULT = do {
    my @arr;
    my $i = 0;
    until $i >= 3 { @arr.push($i); $i = $i + 1 }
    @arr
};
is-deeply UNTIL-RESULT.List, (0, 1, 2),
    'until body runs for side effects in a BEGIN-time do-block';

my constant LOOP-RESULT = do {
    my @arr;
    loop (my $i = 0; $i < 3; $i++) { @arr.push($i) }
    @arr
};
is-deeply LOOP-RESULT.List, (0, 1, 2),
    'C-style loop runs for side effects in a BEGIN-time do-block';

my constant REPEAT-RESULT = do {
    my @arr;
    my $i = 0;
    repeat { @arr.push($i); $i = $i + 1 } while $i < 3;
    @arr
};
is-deeply REPEAT-RESULT.List, (0, 1, 2),
    'repeat-while body runs for side effects in a BEGIN-time do-block';

# vim: expandtab shiftwidth=4
