use Test;

plan 5;

# A sigilless term declaration is installed before its initializer is parsed,
# so the term can refer to itself from within that initializer. This is what
# makes a self-referential lazy binding (such as a Hamming-number sequence)
# work.

{
    my \S := gather {
        take 1;
        take S.head + 10;
    }
    is S[^2].join(','), '1,11', 'a term is visible inside its own initializer';
}

{
    my \Hammings := gather {
        my %i = (2, 3, 5) Z=> (Hammings.iterator for ^3);
        my %n = (2, 3, 5) Z=> 1 xx *;
        loop {
            take my $n := %n{*}.min;
            -> \k { %n{k} = %i{k}.pull-one * k if %n{k} == $n } for (2, 3, 5);
        }
    }
    is Hammings[^10].join(' '), '1 2 3 4 5 6 8 9 10 12',
        'a term self-reference drives a recursive lazy sequence';
}

{
    my \a := 1;
    my \b := a + 1;
    is b, 2, 'a term may reference an earlier term';
}

{
    my Int \typed := 42;
    is typed, 42, 'a typed term binding still works';
}

{
    my \untouched := 5;
    is untouched, 5, 'a term with no self-reference is unaffected';
}
