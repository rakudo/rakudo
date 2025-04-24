#!/usr/bin/env raku

=begin comment

For historic release dates, show information from the release guide.

For future release dates, generate potential dates based on the last
available entry in the release guide.

=end comment

my %dates;

# Grab the raw release info from the release guide

class Release {
    has Date $.date;
    has Int  $.number;
    has Str  $.desc;

    method gist() {
        if $.number {
            return "  {$.date}   Rakudo #{$.number} \"{$.desc}\"";
        } else {
            return "  {$.date} {$.desc}";
        }
    }
}

my %historic;
my $once = True;
for "docs/release_guide.pod".IO.lines -> $line {
     next unless $line ~~ / ^ '  ' ( \d\d\d\d '-' \d\d '-' \d\d ) /;
     if $once {
         $once = False;
         next;
     }
     my $date = Date.new(~$0);
     my $number = 0;
     my $desc = "";

     if $line ~~ / 'Rakudo #' (\d+) ' "' (\d\d\d\d '.' \d\d) '"' / {
         $number = +~$0;
         $desc   = ~$1;
     } elsif $line ~~ / ( \d\d\d\d '.' \d\d '.' \d )/ {
         $desc = ~$0;
     }
     my $release = Release.new(:$date, :$number, :$desc);

     %historic{$date}.push: $release;
}

my ($first,$last,$last-release-number);
for %historic.keys.sort -> $key {
    my $date = Date.new($key);
    $first = $date unless $first;
    next if $date > Date.today;
    $last = $date;
    $last-release-number = %historic{$key}[0].number;
}

sub MAIN ($year = Date.today.year) {
    my $earliest = Date.new($first).year;
    if $year < $earliest {
        note "No support for pre-historic release dates (back to $earliest)";
        exit 1;
    }
    my $last-month;
    for %historic.keys.sort -> $key {
        my $date = Date.new($key);
        next unless $date.year == $year;
        next if $date > Date.today;
        $last-month = $date.month;
        for @(%historic{$key}) -> $rel {
            say $rel;
        }
    }
    my $start-month;
    my $next-release-number;
    if $year < Date.today.year {
        exit; # Done
    } elsif $year == Date.today.year {
        $start-month = $last-month + 1;
        $next-release-number = $last-release-number+1;
    } else {
        $start-month = 1 ;
        $next-release-number = $last-release-number + ($year-$last.year) * 12 + ($start-month - $last.month)
    }

    for $start-month..12 -> $month {
        my $d = Date.new($year, $month, 1);
        ++$d until $d.day-of-week == 6;     # $d is now first Saturday
        $d += 14;                           # ... third Saturday
        say "  $d   Rakudo #$next-release-number";
        $next-release-number++;
    }
}
