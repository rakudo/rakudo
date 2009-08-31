#! perl
# Copyright (C) 2008, The Perl Foundation
# $Id$

=head1 NAME

progress-graph.pl - generate a chart that displays rakudo's progress with
passing tests.

=head1 SYNOPSIS

perl tools/progress-graph.pl [input_file [output_file]]

=head1 DESCRIPTION

Create a chart that displays the number of passed, skipped and TODO tests.

If C<input_file> is given at the command line, it is read and parsed as a CSV
file. if absent F<docs/spectest-progress.csv> is used.

If C<output_file> is given, the image is written to that file. If not, the
file name C<rakudo-tests-%s.png> is used, where C<%s> stands for the last
processed date.

=cut


use strict;
use warnings;
use GD;
use GD::Graph::bars;
use Text::CSV_XS;
use List::Util qw(max sum);
use POSIX qw(log10 ceil);
use Getopt::Long;

# column names
use constant DATE       => 0;
use constant REVISION   => 1;
use constant PASS       => 2;
use constant FAIL       => 3;
use constant TODO       => 4;
use constant SKIP       => 5;
use constant REGR       => 6;
use constant SPEC       => 7;
use constant FILES      => 8;
use constant SPECSKIP   => 9;

use constant MAX_COL    => 9;

my $size = '800x500';

GetOptions
    'size=s'    => \$size,
    or usage();

my $fn = $ARGV[0] || 'docs/spectest-progress.csv';
open my $f, '<', $fn or die "Can't open file '$fn' for reading: $!";
my @data = map [], 0 .. MAX_COL;

my $csv = Text::CSV_XS->new({
        quote_char  => q{"},
        sep_char    => q{,},
    });

my $max = 0;
my @columns_to_plot = (PASS, FAIL, TODO, SKIP, SPECSKIP);
my $rows = 0;

while (<$f>) {
    next if m/^"[a-z]+"/i; # skip header line
    next unless m/\d/;     # empty lines and such
    $csv->parse($_);
    my @cols = $csv->fields();
    push @{$data[0]}, substr $cols[0], 0, 10;
    $cols[SPECSKIP] = $cols[SPEC] - sum @cols[PASS, FAIL, TODO, SKIP];
    for (1..MAX_COL){
        push @{$data[$_]}, $cols[$_];
    }
    $max = max $max, sum @cols[@columns_to_plot];
    $rows++;
}

my $last_date = $data[DATE][-1];

# GD::Graph always prints the last label, which leads to overlapping
# labels. Better remove it.
$data[DATE][-1] = '';

my $p = GD::Graph::bars->new(split m/x/, $size, 2);
no warnings 'qw';
$p->set(
        x_label             => 'Date',
        y_label             => 'Tests',
        title               => 'Rakudo Spectest Progress',
        x_label_skip        => int($rows / 20),
        x_labels_vertical   => 1,
        cumulate            => 1,
        borderclrs          => [undef],
        dclrs               => [qw(#00FF00 #FF0000 #0000FF #FFFF00 #DDDDDD)]
    ) or die $p->error;

$p->set_legend('Pass', 'Fail', 'Todo', 'Regr', 'Spec');
$p->set_x_axis_font(gdSmallFont);
$p->set_y_axis_font(gdLargeFont);

# determine a better y_max_value - GD::Graph wastes much space by default
my $round_to = 10 ** int(log10 $max) / 5;
$max = $round_to * (1 + int($max / $round_to));
$p->set(y_max_value => $max );

my $g = $p->plot([@data[DATE, @columns_to_plot]]) or die $p->error;
my $out_file = $ARGV[1] || "rakudo-tests-$last_date.png";
open my $o, '>', $out_file
    or die "Can't open file `$out_file' for writing: $!";
binmode $o;
print $o $g->png;
close $o;
print "Image written to file '$out_file'\n";

sub usage {
    print <<USAGE;
Usage:
    $0 [--size XXXxYYY] [data_file [output_file]]
Options
    --size  Size of the output image, default is 600x400
USAGE
    exit 1;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
