#! perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

=head1 DESCRIPTION

This tool runs all spectets, except those that C<make spectest> runs (that
means all tests of which we don't know yet if they will pass or not).

For each file that passes at least one test (criterion might change in future)
it prints out a short summary about the status of this file.

This is primarily used to identify tests that could be added to
F<t/spectest.data>, and those that are worth a closer look. But
please don't add them blindly just because they all pass - chances are that
there's a good reason for them not already being included.

This script should be called from the main Rakudo directory (ie
C<languages/rakudo/> relative to parrot).

=cut

use strict;
use warnings;
use TAP::Harness;
use TAP::Parser::Aggregator 3.01;

use File::Find;
use Data::Dumper;

my $parrot = -d 'parrot' ? 'parrot/parrot' : '../../parrot';

my %not_process;
{
    my @not_process = read_specfile('t/spectest.data');
    @not_process{@not_process}  = (1) x  @not_process;
}

print <<KEY;
Key:
[S  ]   = some tests passed
[ P ]   = plan ok (ran all tests)
[  A]   = all passed
      ( passed / planned or ran )
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
           $actually_passed, $planned, $_
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
            exec        => [$parrot, '-G', 'perl6.pbc'],
            merge       => 1,
    });
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
