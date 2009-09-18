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
use Time::Local;

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

# start simple relative benchmarking
my( %times, @interesting_times );
if ( open( my $times, '<', 'docs/test_summary.times') ) {
	while ( <$times> ) {
	    if (/^(.*),(\d+)-(\d+)-(\d+)\s(\d+):(\d+):(\d+),(.*)/) {
	        my ( $testname, $year, $mon, $mday, $hour, $min, $sec, $cusertime )
	            = ( $1, $2, $3, $4, $5, $6, $7, $8 );
	        my $timegm = timegm( $sec, $min, $hour, $mday, $mon-1, $year-1900 );
	        $times{$testname} = [ $timegm, $cusertime ];
	    }
	}
	close $times or die $!;
}
$times{'test startup'} = [ time, 9999 ]; # ignore test startup from previous runs?
open( my $times, '>', 'docs/test_summary.times.tmp') or die "cannot create docs/test_summary.times.tmp: $!";

# Execute all test scripts, aggregate the results, display the failures
$| = 1;
my ( @fail, @plan_hint );
my %plan_per_file;
for my $tfile (@tfiles) {
    my $th;
    open($th, '<', $tfile) || die "Can't read $tfile: $!\n";
    my ($pass,$fail,$todo,$skip,$plan,$abort,$bonus) = (0,0,0,0,0,0,0);
    my $no_plan = 0; # planless may be fine, but bad for statistics
    # http://www.shadowcat.co.uk/blog/matt-s-trout/a-cunning-no_plan/
    while (<$th>) {                # extract the number of tests planned
        if (/^\s*plan\D*(\d+)/) { $plan = $1; last; }
        elsif (/^\s*plan\s+\*;/) { $no_plan = 1; last; }
    }
    close $th or die $!;
    my $tname = $tname{$tfile};
    # repeat the column headings at the start of each Synopsis
    if ( $syn ne substr($tname, 0, 3) ) {
        $syn  =  substr($tname, 0, 3);
        printf( "%s  pass fail todo skip plan\n", ' ' x $max );
        unless ( exists $syn{$syn} ) {
            push @fail, "note: test_summary.pl \@syn does not have $syn";
        }
    }
    $syn{$syn}++;
    printf "%s%s..", $tname, '.' x ($max - length($tname));
    my $cmd = "./perl6 $tfile";
    my ($user1,$system1,$cuser1,$csystem1) = times;
    my @results = split "\n", `$cmd`;  # run the test, @result = all stdout
    my ($user2,$system2,$cuser2,$csystem2) = times;
    my (%skip, %todopass, %todofail);
    for (@results) {
        # pass over the optional line containing "1..$planned"
        if    (/^1\.\.(\d+)/) { $plan = $1 if $1 > 0; next; }
        # ignore lines not beginning with "ok $$test" or "not ok $test"
        next unless /^(not )?ok +(\d+)/;
        if    (/#\s*SKIP\s*(.*)/i) { $skip++; $skip{$1}++; }
        elsif (/#\s*TODO\s*(.*)/i) { $todo++;
            my $reason = $1;
            if (/^ok /) { $todopass{$reason}++ }
            else        { $todofail{$reason}++ }
        }
        elsif (/^not ok +(.*)/)    { $fail++; push @fail, "$tname $1"; }
        elsif (/^ok +\d+/)         { $pass++; }
    }
    my $test = $pass + $fail + $todo + $skip;
    if ($plan > $test) {
        $abort = $plan - $test;
        $fail += $abort;
        push @fail, "$tname aborted $abort test(s)";
    }
    elsif ($plan < $test) {
        $bonus = $test - $plan;
        push @fail, "$tname passed $bonus unplanned test(s)";
    }
    if ($no_plan) {
        push @plan_hint, "'plan *;' could become 'plan $plan;' in $tname";
    }
    printf "%4d %4d %4d %4d %4d\n",
        $pass, $fail, $todo, $skip, $plan;
    $sum{'pass'} += $pass;  $sum{"$syn-pass"} += $pass;
    $sum{'fail'} += $fail;  $sum{"$syn-fail"} += $fail;
    $sum{'todo'} += $todo;  $sum{"$syn-todo"} += $todo;
    $sum{'skip'} += $skip;  $sum{"$syn-skip"} += $skip;
    $sum{'plan'} += $plan;  $sum{"$syn-plan"} += $plan;
    {
        my $f = $tfile;
        $f =~ s/\.rakudo$/.t/;
        $plan_per_file{$f} = $plan;
    }
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
    # track simple relative benchmarking
    {
        my $cuser = $cuser2 - $cuser1;
        if ( $cuser < $times{'test startup'}->[1] ) {
            $times{'test startup'} = [ time, $cuser ];
        }
        if ( not exists( $times{$tname} ) ) { $times{$tname} = [ time, $cuser ]; }
        my $datet_old = $times{$tname}->[0];
        my $cuser_old = $times{$tname}->[1];
        my $diff_sec = abs($cuser - $times{$tname}->[1]);
        if ( $diff_sec >= 0.05 ) {
            push @interesting_times, [ $tname, $datet_old, $cuser_old, time, $cuser, $diff_sec ];
            $times{$tname} = [ time, $cuser ];
        }
        my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime($times{$tname}->[0]);
        printf $times "%s,%04d-%02d-%02d %02d:%02d:%02d,%g\n", $tname,
            $year+1900, $mon+1, $mday, $hour, $min, $sec, $times{$tname}->[1];
    }
} # for my $tfile (@tfiles)

# finish simple relative benchmarking
{
#   use DateTime;
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime;
    printf $times "%s,%04d-%02d-%02d %02d:%02d:%02d,%g\n", 'test startup',
        $year+1900, $mon+1, $mday, $hour, $min, $sec, $times{'test startup'}->[1];
    close $times or die $!;
    rename 'docs/test_summary.times.tmp', 'docs/test_summary.times';
    if ( @interesting_times ) {
        print "----------------\n";
        my $test_startup = $times{'test startup'}->[1];
        print "Minimum test startup ${test_startup}s\n";
        @interesting_times = map  { $_->[0] }             # Schwartzian Transform
                             sort { $b->[1] <=> $a->[1] } # descending
                             map  { [$_, $$_[5]] }        # absolute time difference
                             @interesting_times;
        @interesting_times = @interesting_times[0..($#interesting_times>19?19:$#interesting_times)];
        for my $interesting ( @interesting_times ) {
            my( $tname, $dt1, $cuser1, $dt2, $cuser2, $diff_sec ) = @$interesting;
            my $change = $cuser1 < $cuser2 ? 'slower' : 'faster';
            # The percentage difference is from the previous child user time minus
            # the presumed startup time. Without a check it can divide by zero.
            my $diff_pct = 100;
            if ( $cuser1 != $test_startup ) {
                $diff_pct = 100 * ($cuser2-$cuser1) / ( $cuser1 - $test_startup );
            }
            my $ago = int($dt2 - $dt1);
            my $unit = 'second'; $unit.='s' if $ago!=1;
            if ($ago>60) {
                $ago=int($ago/60); $unit='minute'; $unit.='s' if $ago!=1;
                if ($ago>60) {
                    $ago=int($ago/60); $unit='hour'; $unit.='s' if $ago!=1;
                    if ($ago>24) {
                        $ago=int($ago/24); $unit='day'; $unit.='s' if $ago!=1;
                        if ($ago>7) {
                            $ago=int($ago/7); $unit='week'; $unit.='s' if $ago!=1;
                        }
                    }
                }
            }
            printf "%-38s %.2fs %s (%.1f%%) than %d %s ago\n",
                $tname, $diff_sec, $change, $diff_pct, $ago, $unit;
#           my $d1 = DateTime->from_epoch( epoch => $dt1 );
#           my $d2 = DateTime->from_epoch( epoch => $dt2 );
#           printf "    (%s, %.2f, %s, %.2f)\n", $d1->ymd.' '.$d1->hms,
#               $cuser1, $d2->ymd.' '.$d2->hms, $cuser2;
        }
    }
}

# Calculate plan totals from test scripts grouped by Synopsis and overall.
# This ignores any test list and processes all unfudged files in t/spec/.
# Implementing 'no_plan' or 'plan *' in test scripts would make this
# total inaccurate.
for my $syn (sort keys %syn) {
    my $ackcmd = "ack ^plan t/spec/$syn* -wH"; # some systems use ack-grep
    my @results = `$ackcmd`;       # gets an array of all the plan lines
    my $spec = 0;
    for (@results) {
        my ($fn, undef, $rest) = split /:/, $_;
        if (exists $plan_per_file{$fn}) {
            $spec += $plan_per_file{$fn}
        } else {
            # unreliable because some tests use expressions
            $spec += $1 if $rest =~ /^\s*plan\s+(\d+)/;
        }
    }
    $sum{"$syn-spec"} = $spec;
    $sum{'spec'} += $spec;
}

if (@plan_hint) {
    print "----------------\n";
    foreach (@plan_hint) {
        print "    $_\n";
    }
}

# Show test totals grouped by Synopsys, followed by overall totals
print "----------------\n";
my $sumfmt = qq(%-11.11s %6s,%6s,%6s,%6s,%6s,%6s\n);
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
    print join(',', $ENV{'REV'}, (map { $sum{$_} } @col),
        $filecount), "\n";
    printf "spectest-progress.csv update: " .
        "%d files, %d (%.1f%% of %d) pass, %d fail\n",
        $filecount, $sum{'pass'}, $passpercent, $sum{'spec'}, $sum{'fail'};
}

# List descriptions of the tests that failed
if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) {
        print "$_\n";
    }
}
else {
    print "No failures!\n";
}
