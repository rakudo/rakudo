#!/usr/bin/env perl
use strict;
use warnings;
binmode STDOUT, ':encoding(UTF-8)';
use 5.010;

use Date::Simple qw(today ymd);

my %contrib;

my $last_release = release_date_of_prev_month();
open my $c, '-|', 'git', 'log', "--since=$last_release", '--pretty=format:%an|%cn|%s'
    or die "Can't open pipe to git log: $!";
while (my $line = <$c>) {
    my ($author, $comitter, $msg) = split /\|/, $line, 3;
    $contrib{nick_to_name($author)}++;
    $contrib{nick_to_name($comitter)}++;
    while ($msg =~ /\(([^)]+)\)\+\+/g) {
        $contrib{nick_to_name($1)}++;
    }
    while ($msg =~ /([^\s()]+)\+\+/g) {
        $contrib{nick_to_name($1)}++;
    }
    while ($msg =~ /(courtesy by:?)\s*(\S.*)/i) {
        $contrib{nick_to_name($1)}++;
    }
}
close $c or warn $!;

my @contrib = reverse sort { $contrib{$a} <=> $contrib{$b} } keys %contrib;

print "Contributors to Rakudo since the release on $last_release:\n";
print join(', ', @contrib), "\n";


sub release_date_of_prev_month {
    my $release_date;
    my $last_month = today();
    $last_month-- while $last_month->month == today->month;
    $release_date = ymd(
                        $last_month->year,
                        $last_month->month,
                        1,
                    );
    $release_date++ while $release_date->day_of_week != 2;
    $release_date += 14;
    $release_date++ while $release_date->day_of_week != 4;
    return $release_date;
}

sub nick_to_name_from_CREDITS {
    open my $f, '<', 'CREDITS' or die "Can't open file CREDITS for reading: $!";
    local $/ = '';
    my %nicks;
    while (my $para = <$f>) {
        my @nicks;
        my $name;
        for (split /\n/, $para) {
            if (/^N: (.*)/) {
                $name = $1;
            } elsif (/^U: (.*)/) {
                push @nicks, $1;
            }
        }
        if (defined $name) {
            $nicks{lc $_} = $name for @nicks;
        }
    }
    close $f;
    use Data::Dumper; $Data::Dumper::Sortkeys = 1; print Dumper \%nicks;
    return \%nicks;
}

sub nick_to_name {
    my $nick = shift;
    state $nick_to_name = nick_to_name_from_CREDITS();
    return $nick_to_name->{lc $nick} // $nick;
}

