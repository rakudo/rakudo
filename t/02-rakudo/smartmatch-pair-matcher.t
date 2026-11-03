use Test;
use nqp;

plan 30;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

# A smartmatch or when against a compile-time Pair asks the topic the
# method the key names. The reduction takes the Pair's value from compile
# time only when evaluating it would produce that value and nothing can
# change it.

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

class Flag {
    has $.on is rw = False;
    method Bool() { $!on }
}

ok 5 ~~ :is-prime(1),
    'a smartmatch against a Pair with a constant value asks the topic the method';

nok 4 ~~ :is-prime(1),
    'a smartmatch against a Pair with a constant value fails when the answer differs';

ok 5 !~~ :is-prime(0),
    'a negated smartmatch against a Pair with a constant value asks the topic the method';

nok 5 ~~ :is-prime(try False),
    'a smartmatch against a Pair holding a try compares the value it evaluates to';

nok 7 ~~ (:is-prime(try False)),
    'a smartmatch against a parenthesized Pair holding a try compares the value it evaluates to';

nok 7 ~~ (:is-prime(once False)),
    'a smartmatch against a parenthesized Pair holding a once compares the value it evaluates to';

{
    my $r = 5 ~~ :is-prime(my $y = True);
    is-deeply ($r, $y), (True, True),
        'a smartmatch against a Pair holding a `my` compares its initialized value';
}

{
    my constant P = :is-prime;
    is-deeply (5 ~~ P, 4 ~~ P), (True, False),
        'a smartmatch against a constant Pair asks the topic the method';
}

{
    my constant P = $ = (:is-prime);
    P = (:!is-prime);
    nok 7 ~~ P,
        'a smartmatch against a constant holding a Pair in a container matches its current Pair';
}

todo 'legacy optimizer reads the Pair value at compile time', 4 unless $rakuast;
{
    my constant Q = is-prime => ($ = True);
    Q.value = False;
    nok 7 ~~ Q,
        'a smartmatch against a constant Pair with a container value compares its current value';
}

{
    my constant P = is-prime => [];
    P.value.push(1);
    ok 7 ~~ P,
        'a smartmatch against a constant Pair with an Array value compares its current content';
}

{
    my constant F = is-prime => Flag.new;
    F.value.on = True;
    ok 7 ~~ F,
        'a smartmatch against a constant Pair asks the value for its truth when it runs';
}

{
    my constant A = [];
    A.push(1);
    ok 7 ~~ :is-prime(A),
        'a smartmatch against a Pair holding a constant Array compares its current content';
}

ok (is-prime => 1).Hash ~~ :is-prime(1),
    'a smartmatch of an Associative topic against a Pair looks up the key';

nok (is-prime => 1) ~~ :is-prime(2),
    'a smartmatch of a Pair topic against a Pair compares the values';

given 0 {
    ok 5 ~~ :is-prime($_),
        'a Pair value reading the topic sees the topic of the smartmatch';
}

throws-like { 5 ~~ :no-such-method(1) }, X::Method::NotFound,
    'a smartmatch against a Pair whose key the topic lacks as a method dies';

nok 5 ~~ :is-prime(Int),
    'a smartmatch against a Pair with a type object value compares it as false';

ok 5 ~~ :is-prime{1},
    'a smartmatch against a Pair with a block value compares it as true';

{
    my $t = 3;
    my %h = e => 5;
    is-deeply (%h ~~ :e{ $_ > $t }, %h ~~ :e{ $_ > $t + 6 }), (True, False),
        'a smartmatch of an Associative topic against a Pair with a block value calls the block';
}

ok 5 ~~ :is-prime<x y>,
    'a smartmatch against a Pair with a word list value compares its truth';

is-deeply (5 ~~ :is-prime(More), 5 ~~ :is-prime(Same)), (True, False),
    'a smartmatch against a Pair with an enum value compares its truth';
