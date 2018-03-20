#!/usr/bin/env perl6
use v6;

constant release-number-offset = 2016;
my @first-rn = (
    95,     # 2016
);

sub MAIN ($year = Date.today.year + 1) {
    if $year < 2016 {
        die "No support for pre-historic release dates";
    }
    my $first-rn = @first-rn[release-number-offset - $year]
                   // 12 * ($year - @first-rn - release-number-offset + 1) + @first-rn[*-1];
    for 1..12 -> $month {
        my $d = Date.new($year, $month, 1);
        ++$d until $d.day-of-week == 6;     # $d is now first Saturday
        $d += 14;                           # ... third Saturday
        say "  $d   Rakudo #", $first-rn + $month - 1;
    }
}
