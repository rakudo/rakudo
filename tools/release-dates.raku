#!/usr/bin/env perl6
use v6;

constant release-year-offset = 2016;
my %release-number-checkpoints = (
    2016 =>  95,
    2019 => 130,
    # add more if some releases were skipped
);

sub MAIN ($year = Date.today.year + 1) {
    if $year < 2016 {
        die "No support for pre-historic release dates";
    }
    my $releases-jumped = 0; # from known checkpoint
    my $known-year = $year;
    while not defined %release-number-checkpoints{$known-year} {
        $releases-jumped += 12;
        $known-year--;
    }
    my $first-release-number = %release-number-checkpoints{$known-year} + $releases-jumped;

    for 1..12 -> $month {
        my $d = Date.new($year, $month, 1);
        ++$d until $d.day-of-week == 6;     # $d is now first Saturday
        $d += 14;                           # ... third Saturday
        say "  $d   Rakudo #", $first-release-number + $month - 1;
    }
}
