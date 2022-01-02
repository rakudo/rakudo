#!/usr/bin/env perl
# Copyright (C) 2008, The Perl Foundation.

=head1 DESCRIPTION

This tool runs all spectests, except those that C<make spectest> runs (that
means all tests of which we don't know yet if they will pass or not).

For each file that passes at least one test (criterion might change in future)
it prints out a short summary about the status of this file.

This is primarily used to identify tests that could be added to
F<t/spec/spectest.data>, and those that are worth a closer look. But
please don't add them blindly just because they all pass - chances are that
there's a good reason for them not already being included.

This script should be called from the main Rakudo directory.

=cut

use v5.10;
use strict;
use warnings;
use Getopt::Long;
use FindBin;
use File::Spec;
use TAP::Harness;
use TAP::Parser::Aggregator 3.01;

use File::Find;

my $istty = -t STDOUT;
my %options;
my $base_dir = File::Spec->rel2abs( File::Spec->catdir( $FindBin::Bin, '..' ) );
my $spec_dir = File::Spec->catdir( $base_dir, 't', 'spec' );
my %backend_bin = (
    moar => 'rakudo-m',
    jvm  => 'rakudo-j',
    js   => 'rakudo-js',
);

GetOptions( \%options, 'help!', 'backend=s@', );

$options{backend} = [
    map { die "Unknwon backend '$_'" unless exists $backend_bin{$_}; $_ }
      split( /,/, join( ",", @{ $options{backend} // [qw<moar>] } ) )
];

my %not_process = map { $_ => 1 } read_specfile('t/spec/spectest.data');

# this is a p5 file, don't try to test it.
$not_process{'t/spec/t/fudge.t'} = 1;

print <<'KEY';
A backend column:
    ... passed/planned-or-ran
where the '...' are flags:
    S..   = some tests passed
    .P.   = plan ok (ran all tests)
    ..A   = all passed
KEY

my @wanted;
my %short_path;
my $max_path_len = 32;

find( { wanted => \&queue, no_chdir => 1 }, 't/spec/' );

sub queue {
    return if -d $_;
    return if m/\.sv[nk]/;
    return unless m/\.t$/;
    return if $not_process{$_};

    push @wanted, $_;
    my $splen = length(
        $short_path{$_} = File::Spec->abs2rel(
            File::Spec->rel2abs( $_, $base_dir ), $spec_dir
        )
    );
    $max_path_len = $splen if $splen > $max_path_len;
}

my $sep_line = ( '+' . '-' x 17 ) x scalar( @{ $options{backend} } ) . '+'
  . ( '-' x ( $max_path_len + 2 ) ) . '+';
say "$sep_line\n", ( map { sprintf '| %-15s ', $_ } @{ $options{backend} } ),
  sprintf( "| %-${max_path_len}s |", "" ), "\n$sep_line";

if ( !defined $ENV{TEST_JOBS} || int $ENV{TEST_JOBS} <= 1 ) {
    go($_) for @wanted;
}
else {
    my $jobs_wanted = int $ENV{TEST_JOBS};
    my %running;

    while ( @wanted || %running ) {
        print STDERR scalar(%running), " working, ", scalar(@wanted), " remain\r" if $istty;
        if ( @wanted && $jobs_wanted > keys %running ) {
            my $file = shift @wanted;
            my $pid  = fork;
            if ($pid) {    # parent
                $running{$pid} = $file;
            }
            elsif ( defined $pid ) {    # child
                go($file);
                exit;
            }
            else {
                die "Can't fork: $!";
            }
        }
        else {
            my $pid = wait;
            if ( !defined delete $running{$pid} ) {
                die "reaped unknown child PID '$pid'";
            }
        }
    }
}

say $sep_line;

sub go {
    my $orig = shift @_;

    my $short_path =
      File::Spec->abs2rel( File::Spec->rel2abs( $orig, $base_dir ), $spec_dir );

    my %status;

    foreach my $backend ( @{ $options{backend} } ) {
        my $fudged = qx{t/spec/fudge --keep-exit-code rakudo.$backend $orig};
        chomp $fudged;

        my $compiler_bin =
          File::Spec->catdir( $base_dir, $backend_bin{$backend} );
        my $H   = get_harness($compiler_bin);
        my $agg = TAP::Parser::Aggregator->new();
        $agg->start();
        $H->aggregate_tests( $agg, $fudged );
        $agg->stop();

        # "older" version (prior to 3.16, which isn't released at the time
        # of writing) don't have a planned() method, so fall back on
        # total() instead
        my $planned = eval { $agg->cplanned } // $agg->total;

        my ( $some_passed, $plan_ok, $all_passed ) = ('.') x 3;
        my $actually_passed = $agg->passed - $agg->skipped - $agg->todo;
        $some_passed = 'S' if $actually_passed;
        $plan_ok     = 'P' if !scalar( $agg->parse_errors );
        $all_passed  = 'A' if !$agg->has_errors;
        $status{$backend}{out} = sprintf( "%s%s%s %5d/%-5d",
            $some_passed, $plan_ok, $all_passed, $actually_passed, $planned );
    }

    say "| ",
      join( " | ", map { $status{$_}{out} } @{ $options{backend} } ),
      " | ", sprintf( "%-${max_path_len}s |", $short_path{$orig} );
}

sub read_specfile {
    my $fn = shift;
    my @res;
    open( my $f, '<', $fn ) or die "Can't open file '$fn' for reading: $!";
    while (<$f>) {
        s/\s*\#.*//;    # strip out comments and any spaces before them
        m/(\S+)/ && push @res, "t/spec/$1";
    }
    close $f or die $!;
    return @res;
}

sub get_harness {
    my $rakudo_bin = shift;
    return TAP::Harness->new(
        {
            verbosity => -2,
            exec      => [ $rakudo_bin, qw/-Ilib -I./ ],

            # Not sure if limiting is required as this script is not
            # targetting any kind of automation.
            #[ $^X, 'tools/rakudo-limited.pl', $rakudo_bin, qw/-Ilib -I./ ],
            merge => 1,
        }
    );
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
