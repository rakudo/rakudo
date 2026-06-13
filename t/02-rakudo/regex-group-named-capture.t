use Test;

plan 8;

# A named capture of a bracketed group captures the group as a whole,
# no matter what the group contains. The subrule-aliasing treatment is
# reserved for bare assertions, as in $<x>=<foo> and quantified forms
# thereof. CSS::Grammar's unicode-range rule depends on the quantified
# case: $<from>=[<.xdigit>**1..6] must produce one Match spanning the
# run, not one Match per repetition.

{
    my grammar UR {
        token ur { 'U+' $<from>=[<.xdigit>**1..6] }
    }
    my $m = UR.parse("U+200", :rule<ur>);
    is ~$m<from>, '200',
        'named capture of a group with a quantified atom spans the run';
    nok $m<from> ~~ Positional,
        'and binds a single Match, not a list of repetitions';
}

{
    my grammar Inner {
        token al { $<i>=\w \w* }
        token ur { 'U+' $<x>=[<al>] }
    }
    my $m = Inner.parse("U+abc", :rule<ur>);
    is ~$m<x>, 'abc', 'named capture of a group containing a subrule';
    nok $m<x><i>:exists,
        'the group capture does not adopt the subrule inner captures';
    is ~$m<al><i>, 'a', 'the subrule captures under its own name';
}

{
    my grammar Bare {
        token al { \w }
        token ur { 'U+' $<x>=<al>**3 }
    }
    my $m = Bare.parse("U+abc", :rule<ur>);
    is $m<x>.map(~*).join(','), 'a,b,c',
        'a quantified bare subrule alias captures per repetition';
}

{
    my grammar Quantified {
        token al { \w }
        token ur { 'U+' $<x>=[<al>]**3 }
    }
    my $m = Quantified.parse("U+abc", :rule<ur>);
    is ~$m<x>, 'abc',
        'named capture of a quantified group spans the whole run';
}

{
    my grammar Ratcheted {
        token al { $<i>=\w \w* }
        token ur { 'U+' $<x>=[<al>]: }
    }
    my $m = Ratcheted.parse("U+abc", :rule<ur>);
    nok $m<x><i>:exists,
        'a backtrack-modified group capture does not adopt inner captures';
}

# vim: expandtab shiftwidth=4
