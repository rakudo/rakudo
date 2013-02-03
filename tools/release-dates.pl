use v6;

constant release-number-offset = 2010;
my @first-rn = (
    25,     # 2010
    37,     # 2011
    48,     # 2012
    60,     # 2013
);

sub MAIN ($year = Date.today.year) {
    if $year < 2010 {
        die "No support for pre-historic release dates";
    }
    my $first-rn = @first-rn[release-number-offset - $year]
                   // 12 * ($year - @first-rn - release-number-offset + 1) + @first-rn[*-1];
    for 1..12 -> $month {
        my $d = Date.new($year, $month, 1);
        ++$d until $d.day-of-week == 2;     # $d is now first Tuesday
        $d += 14;                           # ... third Tuesday
        $d += 2;                            # the release is on Thursday
        say "  $d   Rakudo #", $first-rn + $month - 1;
    }
}
