use Test;

plan 8;

# A when against a compile-time Pair asks the topic the method the key
# names. The reduction takes the Pair's value from compile time only when
# evaluating it would produce that value and nothing can change it.

{
    my $fired = False;
    given 5 {
        when :is-prime(try False) { $fired = True }
    }
    nok $fired,
        'a when statement against a Pair holding a try compares the value it evaluates to';
}

{
    my $fired = False;
    given 5 {
        $fired = True when :is-prime(try False);
    }
    nok $fired,
        'a when statement modifier against a Pair holding a try compares the value it evaluates to';
}

{
    my constant A = [];
    A.push(1);
    my $fired = False;
    given 7 {
        when :is-prime(A) { $fired = True }
    }
    ok $fired,
        'a when statement against a Pair holding a constant Array compares its current content';
}

{
    my $fired = False;
    given 5 {
        when :is-prime(Int) { $fired = True }
    }
    nok $fired,
        'a when statement against a Pair with a type object value compares it as false';
}

{
    my $fired = False;
    given 5 {
        when :is-prime{1} { $fired = True }
    }
    ok $fired,
        'a when statement against a Pair with a block value compares it as true';
}

{
    my $t = 3;
    my %h = e => 5;
    my $fired = False;
    given %h {
        when :e{ $_ > $t } { $fired = True }
    }
    ok $fired,
        'a when statement of an Associative topic against a Pair with a block value calls the block';
}

{
    my $fired = False;
    given 5 {
        when :is-prime<x y> { $fired = True }
    }
    ok $fired,
        'a when statement against a Pair with a word list value compares its truth';
}

{
    my $fired = False;
    given 5 {
        when :is-prime(Same) { $fired = True }
    }
    nok $fired,
        'a when statement against a Pair with an enum value compares its truth';
}
