#! perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

=head1 DESCRIPTION

This tool runs the same test that C<make spectest> would, except those that
C<make spectest_regression> runs.

For each file that passes at least one test (criterion might change in future)
it prints out a short summary about the status of this file.

This is primarily used to identify tests that could be added to
F<t/spectest_regression.data>, and those that are worth a closer look. But
please don't add them blindly just because they all pass - chances are that
there's a good reason for them not already being included.

This script should be called from the main Rakudo directory (ie
C<languages/perl6/> relative to parrot).

=cut

use strict;
use warnings;
use TAP::Harness;
use TAP::Parser::Aggregator 3.01;

use File::Find;
use Data::Dumper;


my %not_process;
{
    my @not_process = read_specfile('t/spectest_regression.data');
    @not_process{@not_process}  = (1) x  @not_process;
}

print <<KEY;
Key:
[S  ]   = some tests passed
[ P ]   = plan ok (ran all tests)
[  A]   = all passed
      ( passed / ran )
==================================
KEY

find({ wanted => \&go, no_chdir => 1 }, 't/spec/');

sub go {
    return if -d $_;
    return if m/\.sv[nk]/;
    return unless m/\.t$/;
    return if $not_process{$_};
    my $fudged = qx{t/spec/fudge --keep-exit-code rakudo $_};
    chomp $fudged;
    my $H = get_harness();
    my $agg = TAP::Parser::Aggregator->new();
    $agg->start();
    $H->aggregate_tests($agg, $fudged);
    $agg->stop();

    my ($some_passed, $plan_ok, $all_passed) = (' ', ' ', ' ');
    my $actually_passed = $agg->passed - $agg->skipped - $agg->todo;
    $some_passed = 'S' if $actually_passed;
    $plan_ok     = 'P' if !scalar($agg->parse_errors);
    $all_passed  = 'A' if !       $agg->has_errors;
    printf "[%s%s%s] (% 3d/%-3d) %s\n", $some_passed, $plan_ok, $all_passed,
           $actually_passed, $agg->total, $_
                if $actually_passed;
}

sub read_specfile {
    my $fn = shift;
    my @res;
    open (my $f, '<', $fn) or die "Can't open file '$fn' for reading: $!";
    while (<$f>){
        next if m/#/;
        next unless m/\S/;
        s/\s+\z//;
        push @res, "t/spec/$_";
    }
    return @res;
}

sub get_harness {
    return TAP::Harness->new({
            verbosity   => -2,
            exec        => ['../../parrot', '-G', 'perl6.pbc'],
            merge       => 1,
    });
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
