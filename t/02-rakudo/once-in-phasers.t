use Test;

plan 8;

{
    my $n = 0;
    sub f { ENTER once { $n++ } }
    f() for ^3;
    is $n, 1, 'ENTER once in a sub fires on the first entry only';
}

{
    my $n = 0;
    sub f { ENTER { once { $n++ } } }
    f() for ^3;
    is $n, 1, 'a once inside an ENTER block fires on the first entry only';
}

{
    my @r;
    sub f { ENTER { state $x = 0; @r.push($x++) } }
    f() for ^3;
    is-deeply @r, [0, 1, 2], 'state in an ENTER block persists across entries';
}

{
    my @seen;
    sub f($x) { ENTER { @seen.push($x) } }
    f(1);
    f(2);
    is-deeply @seen, [1, 2], "an ENTER body reads the current entry's arguments";
}

{
    my $n = 0;
    for ^3 { ENTER once { $n++ } }
    is $n, 1, 'ENTER once in a loop body fires on the first iteration only';
}

{
    my $r = await start once { 42 };
    is $r, 42, 'a once as the body of start runs';
}

{
    my $n = 0;
    sub f { once { $n++ } }
    f() for ^3;
    is $n, 1, 'a plain once in a sub still fires once';
}

{
    my @a = gather { for ^3 { once { take 9 }; take 1 } };
    is-deeply @a, [9, 1, 1, 1], 'a once inside gather fires on the first iteration only';
}

# vim: expandtab shiftwidth=4
