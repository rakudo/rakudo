use Test;

# A begin-time evaluated value may be a container. A constant declaration
# binds the container itself, so a Proxy constant keeps calling its FETCH
# on every read, and the same holds for the value of a BEGIN expression.

plan 5;

{
    my $n = 0;
    constant P = Proxy.new(FETCH => method () { ++$n }, STORE => method ($v) { });
    my $first  = P;
    my $second = P;
    isnt $first, $second, 'each read of a Proxy constant calls its FETCH';
    is P.VAR.^name, 'Proxy', 'a Proxy constant binds the Proxy itself';
}

{
    my $n = 0;
    my $b := BEGIN Proxy.new(FETCH => method () { ++$n }, STORE => method ($v) { });
    my $first  = $b;
    my $second = $b;
    isnt $first, $second, 'each read of a BEGIN Proxy value calls its FETCH';
}

{
    constant X = (my $ = 42);
    is X, 42, 'a constant bound to an anonymous scalar reads its value';
    is X.VAR.^name, 'Scalar', 'a constant bound to an anonymous scalar keeps the container';
}

# vim: expandtab shiftwidth=4
