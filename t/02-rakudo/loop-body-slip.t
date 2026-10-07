use Test;

plan 26;

{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; $i %% 2 ?? $i !! Empty }).eager, (2, 4),
        'Empty from the body of from-loop with no condition gives no value';
    is $i, 5, 'Empty from the body of from-loop with no condition does not end it';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; $i %% 2 ?? $i !! [].Slip }).eager, (2, 4),
        'an empty Slip from the body of from-loop with no condition gives no value';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 2; slip($i, $i) }).eager, (1, 1, 2, 2),
        'a Slip from the body of from-loop with no condition gives each of its values';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; slip($i, $i) }).head(3).List, (1, 1, 2),
        'from-loop with no condition gives the values of a Slip from the body as they are taken';
    is $i, 2, 'from-loop with no condition runs the body only as values are taken';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 2; Slip }).eager, (Slip, Slip),
        'the Slip type object from the body of from-loop with no condition is a value';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; slip($i, $i) }, { $i < 2 }).eager, (1, 1, 2, 2),
        'a Slip from the body of from-loop with a condition gives each of its values';
    is $i, 2, 'from-loop with a condition checks it after a Slip from the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; $i %% 2 ?? $i !! Empty }, { $i < 3 }).eager, (2,),
        'Empty from the body of from-loop with a condition gives no value';
    is $i, 3, 'from-loop with a condition checks it after Empty from the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 2; Slip }, { True }).eager, (Slip, Slip),
        'the Slip type object from the body of from-loop with a condition is a value';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; slip($i, $i) }, { $i < 2 }, :repeat).eager, (1, 1, 2, 2),
        'a Slip from the body of a repeat from-loop gives each of its values';
    is $i, 2, 'a repeat from-loop checks its condition after a Slip from the body';
}
{
    my $i = 0;
    is-deeply Seq.from-loop({ last if ++$i > 4; $i %% 2 ?? $i !! Empty }, { $i < 3 }, :repeat).eager, (2,),
        'Empty from the body of a repeat from-loop gives no value';
    is $i, 3, 'a repeat from-loop checks its condition after Empty from the body';
}
{
    my $i = 0;
    is-deeply (do while True { last if ++$i > 3; Empty }).eager, (),
        'Empty from the body of a while loop with a true condition gives no value';
    is $i, 4, 'Empty from the body of a while loop with a true condition does not end it';
}
{
    my $i = 0;
    is-deeply (do while True { last if ++$i > 2; slip($i, $i) }).eager, (1, 1, 2, 2),
        'a Slip from the body of a while loop with a true condition gives each of its values';
}
{
    my $i = 0;
    is-deeply (do loop { last if ++$i > 2; slip($i, $i) }).eager, (1, 1, 2, 2),
        'a Slip from the body of a loop with no condition gives each of its values';
}
{
    my $i = 0;
    is-deeply (do while $i < 2 { last if ++$i > 4; slip($i, $i) }).eager, (1, 1, 2, 2),
        'a Slip from the body of a while loop gives each of its values';
}
{
    my $i = 0;
    is-deeply (do until $i >= 2 { last if ++$i > 4; slip($i, $i) }).eager, (1, 1, 2, 2),
        'a Slip from the body of an until loop gives each of its values';
}
{
    my $i = 0;
    is-deeply (do repeat { last if ++$i > 4; slip($i, $i) } while $i < 2).eager, (1, 1, 2, 2),
        'a Slip from the body of a repeat loop gives each of its values';
}
{
    my $i = 0;
    is-deeply (do repeat { last if ++$i > 4; slip($i, $i) } while False).eager, (1, 1),
        'a Slip from the body of a repeat loop with a false condition gives each of its values';
}
{
    my $i = 0;
    my $runs = 0;
    is-deeply ((++$runs > 4 ?? last() !! slip($i, $i)) while ++$i <= 2).eager, (1, 1, 2, 2),
        'a Slip from a statement with a while modifier gives each of its values';
}
{
    my $i = 0;
    my $runs = 0;
    is-deeply ((++$runs > 4 ?? last() !! slip($i, $i)) until ++$i > 2).eager, (1, 1, 2, 2),
        'a Slip from a statement with an until modifier gives each of its values';
}
