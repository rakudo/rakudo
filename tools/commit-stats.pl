#!/usr/bin/perl
use strict;
use warnings;
use GD::Graph::linespoints;
use List::Util qw(max);
use POSIX qw(log10 ceil);

open my $c, '-|', qw(git log --pretty=format:%ae|%ci) 
    or die "Can't run git log: $!";

my %email;
my %month;

while (<$c>) {
    chomp;
    my ($email, $date) = split /\|/;
    $email{$email}++;
    $month{substr $date, 0, 7}++;
}
close $c or die $!;

{
    my $max = max values %month;
    my $round = 10 ** int(log10 $max);
    $max = $round * (1 + int($max / $round));
    my $label_skip = int .5 + (values %month) / 20;

    my $g = GD::Graph::linespoints->new(600, 400);
    $g->set(
            x_label         => 'Month',
            y_label         => 'Number of commits',
            title           => 'Commits to Rakudo per Month',
            x_label_skip    => $label_skip,
            x_labels_vertical => 1,
            y_max_value     => $max,
            y_min_value     => 0,

    ) or die $g->error;
    my @data;
    my $c = 0;
    for (sort keys %month){
        push @{$data[0]}, $_;
        push @{$data[1]}, $month{$_};
    }
    my $filename = $ARGV[0] || 'commits.png';
    open my $img, '>', $filename
        or die "Can't open `$filename' for writing: $!";
    binmode $img;
    print $img $g->plot(\@data)->png;
    close $img;
}

{
    my $top = 15;
    my $c = 0;
    print "Top $top commit authors\n";
    for (sort { $email{$b} <=> $email{$a} } keys %email) {
        $c++;
        printf "%-2d % 5d  %s\n", $c,  $email{$_}, $_;
        last if $c >= $top;
    }
}
