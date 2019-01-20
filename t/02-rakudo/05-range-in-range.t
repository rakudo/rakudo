use v6;
use Test;

my @true-tests  = [^10,  5], [^Inf,  42], [-Inf .. Inf, Inf], [-Inf..Inf, -Inf];
my @false-tests = [^10, 99], [^Inf, Inf], [-Inf^..^Inf, Inf], [-Inf..Inf,  NaN];

plan 1 + @true-tests + @false-tests;

for @true-tests -> ($range, $thing) {
    is-deeply $range.in-range($thing), True, "$thing is in range $range";
}
for @false-tests -> ($range, $thing) {
    throws-like { $range.in-range($thing) }, X::OutOfRange,
        "$thing is not in range $range";
}

# RT#130452
throws-like { (-∞^..^∞).in-range: 0/0 }, X::OutOfRange,
    message => /'<0/0>'/,
'0/0 is not in -Inf^..^Inf range';
