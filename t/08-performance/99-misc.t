use Test;

plan 3;

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
    ok $took2 < $took1 / 20,
        "was native .sum $took2 at least 20x as fast as $took1 ({$took1/$took2}x)";
}
