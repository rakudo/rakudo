use Test;

plan 21;

# A smartmatch, when, or where against a compile-time type, junction of
# types, or Pair reduces to a cheaper check on the topic. A matcher that
# holds a declaration keeps the full smartmatch, so its initializer runs.

{
    my $x = 5;
    my $r = $x ~~ (my $y = 5);
    is-deeply ($r, $y), (True, 5),
        'a `my` on the right of a smartmatch is initialized';
}

{
    my $x = 5;
    nok $x ~~ (my $y = 6),
        'a smartmatch against a `my` holding another value does not match';
}

{
    my $x = 5;
    my $r = $x ~~ (my Str $y = "5");
    is-deeply ($r, $y), (True, "5"),
        'a smartmatch against a typed `my` matches its value, not its type';
}

{
    my $x = 5;
    my $r = $x !~~ (my $y = 6);
    is-deeply ($r, $y), (True, 6),
        'a `my` on the right of a negated smartmatch is initialized';
}

{
    my $x = 5;
    my $r = $x ~~ (my $y := 5);
    is-deeply ($r, $y), (True, 5),
        'a `my` bound on the right of a smartmatch is bound';
}

{
    my Int $x = 5;
    my $r = $x ~~ (my $y = 6);
    is-deeply ($r, $y), (False, 6),
        'a `my` on the right of a smartmatch with a typed topic is initialized';
}

{
    my $r = 5 ~~ (my $y = 6);
    is-deeply ($r, $y), (False, 6),
        'a `my` on the right of a smartmatch with a literal topic is initialized';
}

{
    my $x = 5;
    my $r = so $x ~~ (my $y = 6) | Str;
    is-deeply ($r, $y), (False, 6),
        'a `my` in a junction of types on the right of a smartmatch is initialized';
}

{
    my $x = 5;
    my $r = so $x ~~ (my $y is default(Int | Str) = Str);
    is-deeply ($r, $y), (False, Str),
        'a `my` defaulting to a junction of types on the right of a smartmatch is initialized';
}

{
    my $r = 5 ~~ (my $p is default(:is-prime) = :!is-prime);
    is-deeply ($r, $p), (False, :!is-prime),
        'a `my` defaulting to a Pair on the right of a smartmatch is initialized';
}

is-deeply (try EVAL 'my $x = 5; $x ~~ my class Foo { }'), False,
    'a smartmatch against a class declared on its right compiles and does not match';

{
    my $seen;
    given 5 {
        when (my $y = 5) { $seen = $y }
    }
    is $seen, 5,
        'a `my` as the matcher of a when statement is initialized';
}

{
    my $fired = False;
    given 5 {
        when (my $y = 6) { $fired = True }
    }
    nok $fired,
        'a when statement does not fire for a `my` matcher holding another value';
}

{
    my $fired = False;
    given 5 {
        when (my $y = 6) | Str { $fired = True }
    }
    nok $fired,
        'a when statement does not fire for a junction of types holding a `my`';
}

{
    my $fired = False;
    given 5 {
        when (my $y is default(Int | Str) = Str) { $fired = True }
    }
    nok $fired,
        'a when statement does not fire for a `my` defaulting to a junction of types';
}

{
    my $seen;
    given 5 {
        when :is-prime(my $y = True) { $seen = $y }
    }
    is-deeply $seen, True,
        'a when statement against a Pair holding a `my` compares its initialized value';
}

{
    given 5 {
        (my $seen = 1) when (my $y = 5);
        is "$seen $y", '1 5',
            'a `my` as the matcher of a when statement modifier is initialized';
    }
}

{
    my $fired = False;
    given 5 {
        $fired = True when (my $y = 6);
    }
    nok $fired,
        'a when statement modifier does not fire for a `my` matcher holding another value';
}

{
    my $fired = False;
    given 5 {
        $fired = True when :is-prime(my $ = True);
    }
    ok $fired,
        'a when statement modifier against a Pair holding a `my` compares its initialized value';
}

{
    sub f(Int $p where (my $y = 6) | Str) { $p }
    throws-like { f(5) }, X::TypeCheck::Binding::Parameter,
        'a where constraint of a junction of types holding a `my` rejects a non-matching argument';
}

{
    sub f(Int $p where (my $y is default(Int | Str) = Str)) { $p }
    throws-like { f(5) }, X::TypeCheck::Binding::Parameter,
        'a where constraint of a `my` defaulting to a junction of types rejects a non-matching argument';
}
