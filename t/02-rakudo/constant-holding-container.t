use Test;

plan 15;

# A constant can hold a container or a mutable aggregate, whose content
# can change after compile time. A condition or matcher naming the
# constant reads that content when it runs.

{
    my constant B = $ = True;
    B = False;
    my $taken;
    if B { $taken = 'then' } else { $taken = 'else' }
    is $taken, 'else',
        'an if condition reads the current content of a constant holding a container';
}

{
    my constant B = $ = True;
    B = False;
    my $taken = 'none';
    $taken = 'then' if B;
    is $taken, 'none',
        'an if statement modifier reads the current content of a constant holding a container';
}

{
    my constant B = $ = True;
    B = False;
    my $taken = 'none';
    $taken = 'else' unless B;
    is $taken, 'else',
        'an unless statement modifier reads the current content of a constant holding a container';
}

{
    my constant B = $ = True;
    B = False;
    is (B ?? 'then' !! 'else'), 'else',
        'a ternary condition reads the current content of a constant holding a container';
}

{
    my constant B = $ = True;
    B = False;
    is-deeply (B && 'and'), False,
        'an && operand reads the current content of a constant holding a container';
}

{
    my constant B = $ = True;
    B = False;
    is-deeply (B || 'or'), 'or',
        'an || operand reads the current content of a constant holding a container';
}

{
    my constant T = $ = Int;
    T = Rat;
    my $fired = False;
    given 5 {
        when T { $fired = True }
    }
    nok $fired,
        'a when statement matches the current type in a constant holding a container';
}

{
    my constant T = $ = Int;
    T = Rat;
    my $fired = False;
    given 5 {
        $fired = True when T;
    }
    nok $fired,
        'a when statement modifier matches the current type in a constant holding a container';
}

{
    my constant J = $ = Int | Str;
    J = Rat;
    my $r = so 5 ~~ J;
    nok $r,
        'a smartmatch under so matches the current content of a constant holding a junction of types';
}

{
    my constant J = $ = Int | Str;
    J = Rat;
    my $fired = False;
    given 5 {
        when J { $fired = True }
    }
    nok $fired,
        'a when statement matches the current content of a constant holding a junction of types';
}

{
    my constant J = $ = Int | Str;
    J = Rat;
    sub f($p where J) { $p }
    throws-like { f(5) }, X::TypeCheck::Binding::Parameter,
        'a where constraint matches the current content of a constant holding a junction of types';
}

{
    my constant A = [];
    A.push(1);
    my $taken;
    if A { $taken = 'then' } else { $taken = 'else' }
    is $taken, 'then',
        'an if condition reads the current content of a constant Array';
}

{
    my constant A = [];
    A.push(1);
    is (A ?? 'then' !! 'else'), 'then',
        'a ternary condition reads the current content of a constant Array';
}

{
    my constant H = {};
    H<a> = 1;
    is-deeply (H && 'and'), 'and',
        'an && operand reads the current content of a constant Hash';
}

{
    my constant T = $ = Int;
    T = Rat;
    is-deeply (5 ~~ T, 5 !~~ T), (False, True),
        'a smartmatch matches the current type in a constant holding a container';
}
