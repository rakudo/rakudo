#! perl
# Copyright (C) 2008, The Perl Foundation.

=head1 DESCRIPTION

This tool runs all spectests, except those that C<make spectest> runs (that
means all tests of which we don't know yet if they will pass or not).

For each file that passes at least one test (criterion might change in future)
it prints out a short summary about the status of this file.

This is primarily used to identify tests that could be added to
F<t/spectest.data>, and those that are worth a closer look. But
please don't add them blindly just because they all pass - chances are that
there's a good reason for them not already being included.

This script should be called from the main Rakudo directory.

=cut

use strict;
use warnings;
use TAP::Harness;
use TAP::Parser::Aggregator 3.01;

use File::Find;

my %not_process = map { $_ => 1 } read_specfile('t/spectest.data');

print <<'KEY';
Key:
[S  ]   = some tests passed
[ P ]   = plan ok (ran all tests)
[  A]   = all passed
      ( passed / planned or ran )
==================================
KEY

my @wanted;

find({ wanted => \&queue, no_chdir => 1 }, 't/spec/');

sub queue {
    return if -d $_;
    return if m/\.sv[nk]/;
    return unless m/\.t$/;
    return if $not_process{$_};

    push @wanted, $_;
}

if ( ! defined $ENV{TEST_JOBS} || int $ENV{TEST_JOBS} <= 1 ) {
    go( $_ ) for @wanted;
}
else {
    my $jobs_wanted = int $ENV{TEST_JOBS};
    my %running;

    while( @wanted || %running ) {
        if ( @wanted && $jobs_wanted > keys %running ) {
            my $file = shift @wanted;
            my $pid = fork;
            if ( $pid ) {                # parent
                $running{ $pid } = $file;
            }
            elsif ( defined $pid ) {     # child
                go( $file );
                exit;
            }
            else {
                die "Can't fork: $!";
            }
        }
        else {
            my $pid = wait;
            if ( ! defined delete $running{ $pid } ) {
                die "reaped unknown child PID '$pid'";
            }
        }
    }
}

sub go {
    my $orig = shift @_;

    my $fudged = qx{t/spec/fudge --keep-exit-code rakudo $orig};
    chomp $fudged;

    my $H = get_harness();
    my $agg = TAP::Parser::Aggregator->new();
    $agg->start();
    $H->aggregate_tests($agg, $fudged);
    $agg->stop();

    # "older" version (prior to 3.16, which isn't released at the time
    # of writing) don't have a planned() method, so fall back on
    # total() instead
    my $planned = eval { $agg->cplanned };
    $planned    =  $agg->total unless defined $planned;

    my ($some_passed, $plan_ok, $all_passed) = (' ', ' ', ' ');
    my $actually_passed = $agg->passed - $agg->skipped - $agg->todo;
    $some_passed = 'S' if $actually_passed;
    $plan_ok     = 'P' if !scalar($agg->parse_errors);
    $all_passed  = 'A' if !       $agg->has_errors;
    printf "[%s%s%s] (% 3d/%-3d) %s\n", $some_passed, $plan_ok, $all_passed,
           $actually_passed, $planned, $orig
                if $actually_passed || ($plan_ok && $planned > 0);
}

sub read_specfile {
    my $fn = shift;
    my @res;
    open (my $f, '<', $fn) or die "Can't open file '$fn' for reading: $!";
    while (<$f>){
        s/\s*\#.*//;   # strip out comments and any spaces before them
        m/(\S+)/ && push @res, "t/spec/$1";
    }
    close $f or die $!;
    return @res;
}

sub get_harness {
    return TAP::Harness->new({
            verbosity   => -2,
            exec        => [$^X, 'tools/perl6-limited.pl'],
            merge       => 1,
    });
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
