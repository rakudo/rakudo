#! perl

# Copyright (C) 2004-2009, The Perl Foundation.
# $Id$

##  The "make spectest" target tells us how many tests we failed
## (hopefully zero!), but doesn't say how many were actually passed.
##  This script runs the spectest tests and summarizes
##  passed, failed, todoed, skipped, executed and planned test results.
##
##  Usage:
##     tools/test_summary.pl [testlist]
##
##  If supplied, C<testlist> identifies an alternate list of tests
##  to use (e.g., t/localtest.data).

use strict;
use warnings;

# Build the list of test scripts to run in @tfiles
my $testlist = $ARGV[0] || 't/spectest.data';
my $fh;
open($fh, '<', $testlist) || die "Can't read $testlist: $!";
my (@tfiles, %tname); # @tfiles lists all test file names before fudging
while (<$fh>) {
    /^ *#/ && next;
    my ($specfile) = split ' ', $_;
    next unless $specfile;
    push @tfiles, "t/spec/$specfile";
}
close $fh or die $!;

# Fudge any Rakudo specific tests by running the fudgeall script
{
    my $cmd = join ' ', $^X, 't/spec/fudgeall', 'rakudo', @tfiles;
    # Fudgeall prints the name of each test script, but changes the
    # name ending to .rakudo instead of .t if tests were fudged.
    print "$cmd\n";
    @tfiles = split ' ', `$cmd`; # execute fudgeall, collect test names
}

# Put test names in %tname, with the 't/spec/' removed from the start
# and truncated to 49 characters. Keep track of the maximum name length.
@tfiles = sort @tfiles;
my $max = 0;
for my $tfile (@tfiles) {
    my $tname = $tfile;
    $tname =~ s{^t/spec/}{};
    $tname = substr($tname, 0, 49);
    if (length($tname) > $max) {
        $max = length($tname);
    }
    $tname{$tfile} = $tname;
}

# Prepare arrays and hashes to gather and accumulate test statistics
my @col = qw(pass fail todo skip plan spec);
my @syn = qw(S02 S03 S04 S05 S06 S09 S10 S11 S12 S13 S14 S16 S17 S28 S29 S32 int);
my %syn; # number of test scripts per Synopsis
my %sum; # total pass/fail/todo/skip/test/plan per Synposis
my $syn;
for $syn (@syn) {
    $syn{$syn} = 0;
    for my $col (@col) {
        $sum{"$syn-$col"} = 0;
    }
}
$syn = ''; # to reliably trigger the display of column headings

# Execute all test scripts, aggregate the results, display the failures
$| = 1;
my @fail;
for my $tfile (@tfiles) {
    my $th;
    open($th, '<', $tfile) || die "Can't read $tfile: $!\n";
    my ($pass, $fail, $todo, $skip, $test, $plan, $abort, $bonus)
        = (0,0,0,0,0,0,0,0);
    while (<$th>) {                # extract the number of tests planned
        if (/^\s*plan\D*(\d+)/) { $plan = $1; last; }
    }                                   # no_plan makes this meaningless
    close $th or die $!;
    my $tname = $tname{$tfile};
    # repeat the column headings at the start of each Synopsis
    if ( $syn ne substr($tname, 0, 3) ) {
        $syn  =  substr($tname, 0, 3);
        printf( "%s  pass fail todo skip test plan\n", ' ' x $max );
        unless ( exists $syn{$syn} ) {
            push @fail, "note: test_summary.pl \@syn does not have $syn";
        }
    }
    $syn{$syn}++;
    printf "%s%s..", $tname, '.' x ($max - length($tname));
    my $cmd = "./perl6 $tfile";
    my @results = split "\n", `$cmd`;  # run the test, @result = all stdout
    my (%skip, %todopass, %todofail);
    for (@results) {
        # pass over the optional line containing "1..$planned"
        if    (/^1\.\.(\d+)/) { $plan = $1 if $1 > 0; next; }
        # ignore lines not beginning with "ok $$test" or "not ok $test"
        next unless /^(not )?ok +(\d+)/;
        $test++;
        if    (/#\s*SKIP\s*(.*)/i) { $skip++; $skip{$1}++; }
        elsif (/#\s*TODO\s*(.*)/i) {
            my $reason = $1;
            $todo++;
            if (/^ok /) { $todopass{$reason}++ }
            else        { $todofail{$reason}++ }
        }
        elsif (/^not ok +(.*)/) {
            $fail++;
            push @fail, "$tname $1";
        }
        elsif (/^ok +\d+/)         { $pass++; }
    }
    # using no_plan, plan 0 or planless testing would break this
    if ($plan > $test) {
        $abort = $plan - $test;
        $fail += $abort;
        push @fail, "$tname aborted $abort test(s)";
        $test = $plan;
    }
    elsif ($plan < $test) {
        $bonus = $test - $plan;
        push @fail, "$tname passed $bonus unplanned test(s)";
    }
    printf "%4d %4d %4d %4d %4d %4d\n",
        $pass, $fail, $todo, $skip, $test, $plan;
    $sum{'pass'} += $pass;  $sum{"$syn-pass"} += $pass;
    $sum{'fail'} += $fail;  $sum{"$syn-fail"} += $fail;
    $sum{'todo'} += $todo;  $sum{"$syn-todo"} += $todo;
    $sum{'skip'} += $skip;  $sum{"$syn-skip"} += $skip;
    # Deprecation notice:
    # Either the 'test' or the 'plan' column could be omitted - they
    # are almost always identical, or the plan is wrong. Discrepancies
    # are now reported per script and in the failure summary at the end.
    # The Synopsis totals at the end, based on @col, show a 'plan' but
    # not a 'test' count.
    $sum{'test'} += $test;  $sum{"$syn-test"} += $test;
    $sum{'plan'} += $plan;  $sum{"$syn-plan"} += $plan;
    for (keys %skip) {
        printf "   %3d skipped: %s\n", $skip{$_}, $_;
    }
    for (keys %todofail) {
        printf "   %3d todo   : %s\n", $todofail{$_}, $_;
    }
    for (keys %todopass) {
        printf "   %3d todo PASSED: %s\n", $todopass{$_}, $_;
    }
    if ($abort) {
        printf "   %3d tests aborted (missing ok/not ok)\n", $abort;
    }
    if ($bonus) {
        printf "   %3d tests more than planned were run\n", $bonus;
    }
} # for my $tfile (@tfiles)

# Calculate plan totals from test scripts grouped by Synopsis and overall.
# This ignores any test list and processes all unfudged files in t/spec/.
# Implementing 'no_plan' or 'plan *' in test scripts would make this
# total inaccurate.
for my $syn (sort keys %syn) {
    my $ackcmd = "ack plan t/spec/$syn* -wh"; # some systems use ack-grep
    my @results = `$ackcmd`;       # gets an array of all the plan lines
    my $spec = 0;
    for (@results) {
        $spec += $1 if /^\s*plan\s+(\d+)/; # unreliable because some
    }                                      # plans use expressions
    $sum{"$syn-spec"} = $spec;
    $sum{'spec'} += $spec;
}

# Show test totals grouped by Synopsys, followed by overall totals
print "----------------\n";
my $sumfmt = qq(%-9.9s %6s,%6s,%6s,%6s,%6s,%6s\n);
printf $sumfmt, qq{"Synopsis",}, map { qq{"$_"} } @col;
for my $syn (sort keys %syn) {
    printf $sumfmt, qq{"$syn",}, map { $sum{"$syn-$_"} } @col;
}
my $total = scalar(@tfiles).' regression files';
printf $sumfmt, qq{"total",}, map { $sum{$_} } @col;
print "----------------\n";

# Optionally show the statistics that can be manually appended to
# docs/spectest-progress.csv
if ($ENV{'REV'}) {
    my @gmt = gmtime;
    my $testdate = sprintf '"%4d-%02d-%02d %02d:%02d"', $gmt[5]+1900,
        $gmt[4]+1, $gmt[3], $gmt[2], $gmt[1];
    my $filecount = scalar(@tfiles);
    my $passpercent = 100 * $sum{'pass'} / $sum{'spec'};
    print join(',', $testdate, $ENV{'REV'}, (map { $sum{$_} } @col),
        $filecount), "\n";
    printf "spectest-progress.csv update: " .
        "%d files, %d (%.1f%% of %d) passing, %d failing\n",
        $filecount, $sum{'pass'}, $passpercent, $sum{'spec'}, $sum{'fail'};
}

# List descriptions of the tests that failed
if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) {
        print "    $_\n";
    }
}
else {
    print "No failures!";
}
