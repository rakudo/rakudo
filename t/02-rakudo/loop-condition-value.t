use Test;
use nqp;

plan 33;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

{
    my @a = 1, 2, 3;
    is-deeply Seq.from-loop(-> $x { $x * 10 }, { @a.shift }, :pass-condition).List, (10, 20, 30),
        'from-loop with pass-condition passes the value of the condition to the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop(-> $x = 'none' { $x }, { $i++ < 2 }).List, ('none', 'none'),
        'from-loop without pass-condition passes nothing to the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ $i }, { $i++ >= 2 }, :until).List, (1, 2),
        'from-loop with until runs the body while the condition is false';
}
{
    my @a = 0, '', 3;
    is-deeply Seq.from-loop(-> $x { $x }, { @a.shift }, :until, :pass-condition).List, (0, ''),
        'from-loop with until and pass-condition passes the value of the condition to the body';
}
{
    my @a = 1, 2;
    is-deeply Seq.from-loop(-> $x { $x }, { @a.shift }, :repeat, :pass-condition).List, (Mu, 1, 2),
        'a repeat from-loop with pass-condition passes Mu before the condition is tested';
}
{
    my @a = 0, 0, 1;
    is-deeply Seq.from-loop(-> $x { $x }, { @a.shift }, :repeat, :until, :pass-condition).List,
        (Mu, 0, 0),
        'a repeat from-loop with until and pass-condition passes the value of the condition to the body';
}
{
    my @a = 1, 2;
    is-deeply Seq.from-loop(-> $x { $x * 10 }, { @a.shift }, -> { }, :pass-condition).List, (10, 20),
        'from-loop with an afterwards block and pass-condition passes the value of the condition to the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ $i }, { $i++ >= 2 }, -> { }, :until).List, (1, 2),
        'from-loop with an afterwards block and until runs the body while the condition is false';
}
{
    my @a = 1, 2;
    is-deeply Seq.from-loop(-> *@x { @x.List }, { @a.shift }, :pass-condition).List, ((1,), (2,)),
        'from-loop with pass-condition passes the value of the condition to a slurpy body';
}

{
    my @a = 1, 2;
    my @seen;
    while @a.shift -> $x { LAST { }; @seen.push($x) }
    is-deeply @seen, [1, 2],
        'a pointy block with a LAST phaser of a while loop gets the value of the condition';
}
{
    my @a = 0, '', 1;
    my @seen;
    until @a.shift -> $x { LAST { }; @seen.push($x) }
    is-deeply @seen, [0, ''],
        'a pointy block with a LAST phaser of an until loop gets the value of the condition';
}
{
    my class CountedTruth {
        has $.left = 2;
        method Bool { $!left-- > 0 }
    }
    my $condition = CountedTruth.new;
    my $runs = 0;
    while $condition -> $x { LAST { }; $runs++ }
    is $runs, 2, 'a while loop with a pointy block and a LAST phaser tests its condition once each time';
}
{
    my @a = 1, 2;
    my @log;
    while @a.shift -> $x { LAST { @log.push('L') }; NEXT { @log.push('N') }; @log.push($x) }
    is-deeply @log, [1, 'N', 2, 'N', 'L'],
        'a pointy block with LAST and NEXT phasers of a while loop gets the value of the condition';
}
{
    my @a = 0, '', 1;
    my @seen;
    until @a.shift { LAST { }; @seen.push($^x) }
    is-deeply @seen, [0, ''],
        'a block with a placeholder and a LAST phaser of an until loop gets the value of the condition';
}

{
    my int $i = 0;
    my @seen;
    while $i < 2 -> $x { $i++; @seen.push($x) }
    is-deeply @seen, [True, True],
        'a pointy block of a while loop with a native condition gets the value of the condition';
}
{
    my int $i = 2;
    my $seen;
    if $i < 5 -> $x { $seen = $x }
    is-deeply $seen, True,
        'a pointy block of an if with a native condition gets the value of the condition';
}
{
    my int $i = 9;
    my $seen;
    if $i < 5 { } elsif $i < 10 -> $x { $seen = $x }
    is-deeply $seen, True,
        'a pointy block of an elsif with a native condition gets the value of the condition';
}
{
    my int $i = 2;
    my $seen;
    if $i > 5 { } else -> $x { $seen = $x }
    is-deeply $seen, False,
        'a pointy block of an else after a native condition gets the value of the condition';
}
{
    my int $i = 5;
    my $seen;
    unless $i < 2 -> $x { $seen = $x }
    is-deeply $seen, False,
        'a pointy block of an unless with a native condition gets the value of the condition';
}
{
    my int $i = 2;
    my $seen;
    { $seen = $^x } if $i < 5;
    is-deeply $seen, True,
        'a block with a placeholder before an if with a native condition gets the value of the condition';
}
{
    my int $i = 5;
    my $seen;
    { $seen = $^x } unless $i < 2;
    is-deeply $seen, False,
        'a block with a placeholder before an unless with a native condition gets the value of the condition';
}

if $rakuast {
    {
        my @a = 1, 2, 3;
        is-deeply (while @a.shift -> $x { $x * 10 }).List, (10, 20, 30),
            'a pointy block of a while loop giving values gets the value of the condition';
    }
    {
        my @a = 1, 2, 3;
        my @values = do while @a.shift -> $x { $x * 10 };
        is-deeply @values, [10, 20, 30],
            'a pointy block of a do while loop gets the value of the condition';
    }
    {
        my @a = 0, '', 3;
        is-deeply (until @a.shift -> $x { $x }).List, (0, ''),
            'a pointy block of an until loop giving values gets the value of the condition';
    }
    {
        my @a = 1, 2;
        is-deeply (repeat -> $x { $x } while @a.shift).List, (Mu, 1, 2),
            'a pointy block of a repeat while loop giving values gets the value of the condition';
    }
    {
        my @a = 0, 0, 1;
        is-deeply (repeat -> $x { $x } until @a.shift).List, (Mu, 0, 0),
            'a pointy block of a repeat until loop giving values gets the value of the condition';
    }
    {
        my @a = 1, 2, 3;
        is-deeply (while @a.shift -> $x { NEXT { }; $x * 10 }).List, (10, 20, 30),
            'a pointy block with a NEXT phaser of a while loop giving values gets the value of the condition';
    }
    {
        my @a = 0, '', 3;
        is-deeply (until @a.shift -> $x { NEXT { }; $x }).List, (0, ''),
            'a pointy block with a NEXT phaser of an until loop giving values gets the value of the condition';
    }
    {
        my $runs = 0;
        is-deeply (while 1 -> $x { last if $runs++ == 2; $x }).List, (1, 1),
            'a pointy block of a while loop giving values gets the value of a constant condition';
    }
    {
        my $runs = 0;
        is-deeply (until False -> $x { last if $runs++ == 2; $x }).List, (False, False),
            'a pointy block of an until loop giving values gets the value of a constant condition';
    }
    {
        my @a = 1, 2;
        is-deeply (while @a.shift { $^a * 10 }).List, (10, 20),
            'a block with a placeholder of a while loop giving values gets the value of the condition';
    }
    {
        my @a = 0, '', 3;
        is-deeply (until @a.shift { $^x }).List, (0, ''),
            'a block with a placeholder of an until loop giving values gets the value of the condition';
    }
    {
        my @a = 1, 2, 3;
        my @seen;
        while @a.shift -> $x { UNDO { }; @seen.push($x) }
        is-deeply @seen, [1, 2, 3],
            'a pointy block with an UNDO phaser of a while loop gets the value of the condition';
    }
}
else {
    skip 'dies on the legacy frontend', 12;
}

# vim: expandtab shiftwidth=4
