use Test;

plan 8;

# Role parameterization arguments are evaluated at BEGIN time. Pairs,
# hash composers, and contextualizers must interpret there, the way
# simple literals and lists already do; Cro::HTTP parameterizes a role
# with a %(...) of timeout phases.

{
    role R1[%h] { method opts { %h } }
    class C1 does R1[%(a => 1, b => 2)] { }
    is C1.opts<a> + C1.opts<b>, 3, 'role parameterized with a %(...) hash';
}

{
    role R2[%h] { method n { %h.elems } }
    class C2 does R2[%()] { }
    is C2.n, 0, 'role parameterized with an empty %()';
}

{
    role R3[$x] { method v { $x } }
    class C3 does R3[(a => 1)] { }
    is C3.v.raku, Pair.new('a', 1).raku, 'role parameterized with a => pair';
}

{
    role R4[%h] { method v { %h<a> } }
    class C4 does R4[{ a => 5 }] { }
    is C4.v, 5, 'role parameterized with a { } hash composer';
}

{
    role R5[@a] { method v { @a } }
    class C5 does R5[@(1, 2, 3)] { }
    is C5.v.join(','), '1,2,3', 'role parameterized with an @(...) list';
    class C5e does R5[@()] { }
    is C5e.v.elems, 0, 'role parameterized with an empty @()';
}

{
    role R6[$x] { method v { $x } }
    class C6 does R6[$(42)] { }
    is C6.v, 42, 'role parameterized with a $(...) item';
}

{
    role R7[%h] { method v { %h<body> } }
    class C7 does R7[%(connection => 60, body => Inf)] { }
    is C7.v, Inf, 'role parameterized with a mixed-value %(...), as Cro does';
}

# vim: expandtab shiftwidth=4
