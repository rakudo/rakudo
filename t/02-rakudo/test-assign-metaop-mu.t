use Test;

plan 6;

{
    my Mu $x;
    $x //= 42;
    is $x, 42, '//= assigns into an undefined Mu-typed container';
}

{
    my Mu $x;
    $x ||= 7;
    is $x, 7, '||= assigns into a false Mu-typed container';
}

{
    my Mu $x = 5;
    $x //= 99;
    is $x, 5, '//= leaves a defined Mu-typed container alone';
}

{
    sub f(Mu :$dir is copy) { $dir //= 'default'; $dir }
    is f(), 'default', '//= works on a Mu-typed parameter';
}

# A literal Mu on the right routes to the _VALUE helper, which binds the
# value rather than thunking it.
{
    my $x;
    $x //= Mu;
    is $x.^name, 'Mu', '//= assigns a Mu value on the right';
}

{
    my $x = 0;
    $x ||= Mu;
    is $x.^name, 'Mu', '||= assigns a Mu value on the right';
}

# vim: expandtab shiftwidth=4
