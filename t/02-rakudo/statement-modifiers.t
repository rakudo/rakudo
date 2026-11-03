use Test;

plan 48;

{
    my $i = 0;
    is-deeply (try EVAL(q[(1 + 2 if False while $i++ < 3).List])), (),
        'a false if with a while modifier gives no values';
}
{
    my $i = 0;
    is-deeply (try EVAL(q[($i * 10 if $i > 1 while $i++ < 3).List])), (20, 30),
        'an if with a while modifier is tested on each iteration';
}
{
    my $i = 0;
    is-deeply (try EVAL(q[($i unless $i < 2 until $i++ >= 3).List])), (2, 3),
        'an unless with an until modifier is tested on each iteration';
}
is-deeply (try EVAL(q[my $n = 0; my $i = 0; my @r = ($i * 10 if ++$n while $i++ < 3); ($n, @r)])),
    (3, [10, 20, 30]),
    'an if with a while modifier is evaluated once per iteration';
is-deeply (try EVAL(q[my $n = 0; my $i = 0; my @r = ($i * 10 unless ++$n > 5 until $i++ >= 3); ($n, @r)])),
    (3, [10, 20, 30]),
    'an unless with an until modifier is evaluated once per iteration';
is-deeply (try EVAL(q[my $n = 0; my $i = 0; my @r = ({ $i * 10 } if ++$n while $i++ < 3); ($n, @r)])),
    (3, [10, 20, 30]),
    'an if on a block with a while modifier is evaluated once per iteration';
is-deeply (try EVAL(q[my $n = 0; my @r = ({ $_ * 10 } if ++$n for 1..3); ($n, @r)])),
    (3, [10, 20, 30]),
    'an if on a block with a for modifier is evaluated once per iteration';

is-deeply (try EVAL(q[({ 7 } if False for 1..3).List])), (),
    'a block with a false if and a for modifier gives no values';
is-deeply (try EVAL(q[({ $_ * 10 } if $_ > 1 for 1..3).List])), (20, 30),
    'a block with an if and a for modifier runs where the if holds, given the topic';
is-deeply (try EVAL(q[({ $_ } unless $_ == 2 for 1..3).List])), (1, 3),
    'a block with an unless and a for modifier runs where the unless condition is false';
is-deeply (try EVAL(q[({ $_ * 10 } with 42 for 1..2).List])), (420, 420),
    'a block with a with and a for modifier is given the tested value';
is-deeply (try EVAL(q[({ $_.^name } without Str for 1..2).List])), ('Str', 'Str'),
    'a block with a without and a for modifier is given the tested value';
is-deeply (try EVAL(q[({ $_ * 10 } when 2 for 1..3).List])), (20,),
    'a block with a when and a for modifier runs where the topic matches';
is-deeply (try EVAL(q[my @seen; { @seen.push: $_ } if $_ > 1 for 1..3; @seen])), [2, 3],
    'a sunk block with an if and a for modifier runs where the if holds';
is-deeply (try EVAL(q[my @seen; { @seen.push: $_; NEXT @seen.push: 'next' } if True for 1..2; @seen])),
    [1, 2],
    'a block with an if and a for modifier is not the loop body, so its NEXT phaser does not fire';
{
    my @seen;
    (1..2).map({ @seen.push: $_ }) if True for 1;
    is-deeply @seen, [1, 2], 'the Seq of a sunk statement with an if and a for modifier is sunk';
}
{
    my @seen;
    (1..$_).map({ @seen.push: $_ }) with 2 for 1;
    is-deeply @seen, [1, 2],
        'the Seq of a sunk statement with a with and a for modifier is sunk with the tested value as topic';
}
is-deeply (try EVAL(q[my @seen; { last if $_ == 3; @seen.push: $_ } if $_ > 1 for 1..4; @seen])), [2],
    'a last in a block with an if and a for modifier leaves the loop';
is-deeply (try EVAL(q[my $i = 0; my @seen; { last if $i == 3; @seen.push: $i } if $i > 1 while $i++ < 5; @seen])),
    [2],
    'a last in a block with an if and a while modifier leaves the loop';

is-deeply (try EVAL(q[{ 7 } if False given 1])), Empty,
    'a block with a false if and a given modifier gives Empty';
is-deeply (try EVAL(q[{ $_ * 10 } if True given 2])), 20,
    'a block with a true if and a given modifier is given the topic';
is-deeply (try EVAL(q[{ $_ * 10 } unless False given 2])), 20,
    'a block with a false unless and a given modifier is given the topic';
is-deeply (try EVAL(q[({ $_ * 10 } if $_ > 5 given 2)])), Empty,
    'a block with a given modifier gives Empty where the topic fails the if';
is-deeply (try EVAL(q[({ $_ * 10 } if $_ > 1 given 2)])), 20,
    'a block with a given modifier runs where the topic passes the if';
is-deeply (try EVAL(q[my @seen; { @seen.push: $_ * 10 } with 42 given 2; @seen])), [420],
    'a sunk block with a with and a given modifier is given the tested value';
is-deeply (try EVAL(q[my @seen; { @seen.push: $_ * 10 } when 2 given 2; @seen])), [20],
    'a sunk block with a when and a given modifier is given the topic';

is-deeply (try EVAL(q[my $i = 0; my @seen; { @seen.push: $i } if $i > 1 while $i++ < 3; @seen])), [2, 3],
    'a sunk block with an if and a while modifier runs where the if holds';
is-deeply (try EVAL(q[my $i = 0; my @seen; { @seen.push: $i } if $i > 1 until $i++ >= 3; @seen])), [2, 3],
    'a sunk block with an if and an until modifier runs where the if holds';
{
    my $i = 0;
    is-deeply (try EVAL(q[({ $i * 10 } if $i > 1 while $i++ < 3).List])), (20, 30),
        'a block with an if and a while modifier gives a value where the if holds';
}
is-deeply (try EVAL(q[my $i = 0; ({ $_ * 10 } with 42 while $i++ < 2).List])), (420, 420),
    'a block with a with and a while modifier is given the tested value';
is-deeply (try EVAL(q[
        sub f(@seen) { my $i = 0; { @seen.push: $i } if $i > 1 while $i++ < 3 }
        my @seen;
        my \result = f(@seen);
        (result, @seen)
    ])),
    (Nil, [2, 3]),
    'a block with an if and a while modifier as the last statement of a routine runs where the if holds';
is-deeply (try EVAL(q[
        sub f { my $i = 0; my @seen; { @seen.push: $i } if $i > 1 while $i++ < 3; @seen }
        BEGIN f()
    ])),
    [2, 3],
    'a block with an if and a while modifier runs at BEGIN time';

is-deeply (try EVAL(q[(-> $x { $x * 10 } with 42 for 1..2).List])), (420, 420),
    'a pointy block with a with and a for modifier is given the tested value';
is-deeply (try EVAL(q[my @seen; -> $x { @seen.push: $x } with 42 for 1..2; @seen])), [42, 42],
    'a sunk pointy block with a with and a for modifier is given the tested value';
is-deeply (try EVAL(q[({ $^a * 10 } if True for 1..2).List])), (10, 10),
    'a placeholder block with an if and a for modifier is given the condition';
is-deeply (try EVAL(q[my @seen; { @seen.push: $^a } if 3 for 1..2; @seen])), [3, 3],
    'a sunk placeholder block with an if and a for modifier is given the condition';
is-deeply (try EVAL(q[({ $^a * 10 } when 2 for 1..3).List])), (10,),
    'a placeholder block with a when and a for modifier is given the smartmatch result';
{
    my $i = 0;
    is-deeply (try EVAL(q[({ $^a * 10 } with 42 while $i++ < 2).List])), (420, 420),
        'a placeholder block with a with and a while modifier is given the tested value';
}
is-deeply (try EVAL(q[my $i = 0; my @seen; { @seen.push: $^a } with 42 while $i++ < 2; @seen])), [42, 42],
    'a sunk placeholder block with a with and a while modifier is given the tested value';
is-deeply (try EVAL(q[({ $^a * 10 } if True given 2)])), 10,
    'a placeholder block with an if and a given modifier is given the condition';
is-deeply (try EVAL(q[(-> $x { $x * 10 } with 42 given 2)])), 420,
    'a pointy block with a with and a given modifier is given the tested value';

is-deeply (try EVAL(q[my $x = ({ 7 } if True); $x])), 7,
    'a block with an if in parentheses runs';
is-deeply (try EVAL(q[({ $_ * 10 } with 42)])), 420,
    'a block with a with in parentheses is given the tested value';
is-deeply (try EVAL(q[({ $_ * 10 } given 2)])), 20,
    'a block with a given in parentheses is given the topic';
is-deeply (try EVAL(q[({ $^a * 10 } given 2)])), 20,
    'a placeholder block with a given in parentheses is given the topic';
is-deeply (try EVAL(q[my $y = 1; my $x = ({ my $y = 7; $y } if True); ($x, $y)])), (7, 1),
    'a block with an if in parentheses keeps its own lexicals';
is-deeply (try EVAL(q[my @c; for 1..3 { my $v = $_; @c.push(({ -> { $v } } if True)) }; @c».()])),
    [1, 2, 3],
    'a closure from a block with an if in parentheses captures the outer lexical of its iteration';
is-deeply (try EVAL(q[my @c; for 1..3 { @c.push(({ my $v = $_ * 10; -> { $v } } if True)) }; @c».()])),
    [10, 20, 30],
    'a closure from a block with an if in parentheses captures the block lexical of its iteration';
