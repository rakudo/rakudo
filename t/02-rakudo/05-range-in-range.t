use v6;
use Test;

my @true-tests  = [^10,  5], [^Inf,  42], [-Inf .. Inf, Inf], [-Inf..Inf, -Inf];
my @false-tests = [^10, 99], [^Inf, Inf], [-Inf^..^Inf, Inf], [-Inf..Inf,  NaN];
plan @true-tests + @false-tests;

for @true-tests -> ($range, $thing) {
    is-deeply $range.in-range($thing), True, "$thing is in range $range";
}
for @false-tests -> ($range, $thing) {
    throws-like { $range.in-range($thing) }, X::OutOfRange,
        "$thing is in not in range $range";
}
