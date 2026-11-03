use Test;

plan 6;

# An `our`-scoped operator variable must use one container for both its
# lexical and its package entry, so a value assigned to it is visible through
# the package (which is what `is export` hands to importers). The package key
# has to carry the operator's colonpair, matching the lexical name.

{
    sub c(@a, @b) { |@a, |@b }
    our &infix:<qq> = &c;
    ok OUR::{'&infix:<qq>'} === &c,
        'the package entry holds the value assigned to the operator';
    is-deeply (1, 2) qq (3, 4), (1, 2, 3, 4),
        'the assigned operator works when used';
}

{
    sub double($n) { $n * 2 }
    our &prefix:<§> = &double;
    ok OUR::{'&prefix:<§>'} === &double,
        'a prefix operator variable also reaches its package entry';
    is §21, 42, 'the assigned prefix operator works';
}

{
    module M {
        sub c(@a, @b) { |@a, |@b }
        our &infix:<qq> is export = &c;
    }
    my $exported = M::EXPORT::DEFAULT::{'&infix:<qq>'};
    isa-ok $exported, Sub,
        'an exported operator variable exports the assigned routine, not the empty container';
    is-deeply $exported((1, 2), (3, 4)), (1, 2, 3, 4),
        'the exported operator is invocable';
}
