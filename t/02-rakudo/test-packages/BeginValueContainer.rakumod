unit module BeginValueContainer;

my $n = 0;
constant PROX is export = Proxy.new(
    FETCH => method ()   { ++$n },
    STORE => method ($v) { },
);
constant SCAL is export = (my $ = 42);
