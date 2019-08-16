use Test;

my $skip = %*ENV<RAKUDO_SKIP_TIMING_TESTS> ?? 1 !! 0;

plan 4 - $skip;

# https://github.com/rakudo/rakudo/issues/1488
{
    my class Foo {
        method Stringy {
            die "missed optimization"
        }
    };
    my $f := Foo.new;
    is-deeply $f cmp ($ = $f), Same, 'eqaddr optimization for cmp exists'
}

{
    my int @a = ^2_000_000;
    my $then = now;
    my $result1 = @a.sum;
    my $took1 = now - $then;

    $then = now;
    my $result2 = @a.sum(:wrap);
    my $took2 = now - $then;

    is $result1, $result2, "is $result1 == $result2";
    ok $took2 < $took1 / 10,
        "was native .sum $took2 at least 10x as fast as $took1 ({$took1/$took2}x)";
}

unless $skip { # https://github.com/rakudo/rakudo/issues/1740
    my $t-plain = { (^∞).grep(*.is-prime)[1000];       now - ENTER now }();
    my $t-hyper = { (^∞).hyper.grep(*.is-prime)[1000]; now - ENTER now }();
    cmp-ok $t-hyper, '≤', $t-plain*10,
        'hypered .grep .is-prime is not hugely slower than plain grep';
}
