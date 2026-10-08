use Test;

plan 9;

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

# vim: expandtab shiftwidth=4
