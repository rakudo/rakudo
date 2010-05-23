#! perl

# Copyright (C) 2004-2010, The Perl Foundation.
# $Id$

##  The "make spectest" target tells us how many tests we failed
##  (hopefully zero!), but doesn't say how many were actually passed.
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
use Time::HiRes;

my $benchmark;
# Comment out the next line to skip benchmarking; see docs below
$benchmark = Simple::Relative::Benchmarking::begin();    # defined below

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
    # Fudgeall prints the name of each test script, but changes the name
    # ending to .rakudo instead of .t if tests were fudged.
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
my @syn = qw(S02 S03 S04 S05 S06 S07 S09 S10 S11 S12 S13 S14 S16 S17 S28 S29 S32 int);
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
    # Repeat the column headings at the start of each Synopsis
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
    # Run the test, collecting all stdout in @results
    my @results = split "\n", qx{$cmd};
    my (%skip, %todopass, %todofail);
    my ($time1, $time2, $testnumber, $test_comment ) = ( 0, 0, 0, '' );
    my @times = (); my @comments = ();
    for (@results) {
        # Pass over the optional line containing "1..$planned"
        if    (/^1\.\.(\d+)/)      { $plan = $1 if $1 > 0; next; }
        # Handle lines containing timestamps
        if    (/^# t=(\d+\.\d+)/)  {
            # Calculate the per test execution time
            $time2 = $time1;
            $time1 = $1;
            my $microseconds = int( ($time1 - $time2) * 1_000_000 );
            if ( $testnumber > 0 ) {
                $times[$testnumber] = $microseconds;
                $comments[$testnumber] = $test_comment;
                $testnumber = 0;
            }
            next;
        }
        # Ignore lines not beginning with "ok $$test" or "not ok $test"
        next unless /^(not )?ok +(\d+)/;
        if    (/#\s*SKIP\s*(.*)/i) { $skip++; $skip{$1}++; }
        elsif (/#\s*TODO\s*(.*)/i) { $todo++;
            my $reason = $1;
            if (/^ok /) { $todopass{$reason}++ }
            else        { $todofail{$reason}++ }
        }
        elsif (/^not ok +(.*)/)    { $fail++; push @fail, "$tname $1"; }
        elsif (/^ok +(\d+) - (.*)$/) {
            $pass++; $testnumber = $1; $test_comment = $2;
        }
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
    defined $benchmark && $benchmark->log_script_times($tfile,\@times,\@comments);
} # for my $tfile (@tfiles)
defined $benchmark && $benchmark->end(); # finish simple relative benchmarking

# Calculate plan totals from test scripts grouped by Synopsis and overall.
# This ignores any test list and processes all unfudged files in t/spec/.
# Implementing 'no_plan' or 'plan *' in test scripts makes this total
# inaccurate.
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

# End of main program

#-------------------- Simple Relative Benchmarking ---------------------

package Simple::Relative::Benchmarking;

sub begin {       # this constructor starts simple relative benchmarking
    my $timings = shift || 5;    # number of timings to keep (default 5)
    my $self = {};
    my @test_history;
    $self->{'Timings'} = $timings;
    $self->{'Last_test_loaded'} = '';

    if ( open( $self->{'file_in'}, '<', 'docs/test_summary.times') ) {
        my $file_in = $self->{'file_in'};
        my $line = <$file_in>; chomp $line;
        if ( $line =~ m/{"test_.+":\[/i ) { # should be Test_history
            $line = <$file_in>; chomp $line;
            while ( $line =~ m/\s\s(.+\d\d\d\d-\d\d-\d\d.\d\d:\d\d:\d\d.+)/ ) {
                my $history_line = $1;
                $history_line =~ s/,$//; # trim possible trailing comma
                push @test_history, $history_line;
                $line = <$file_in>; chomp $line;
            } # ends on the ' ],' line after the test_history
            $line = <$file_in>; chomp $line;
            if ( $line =~ m/ "test_microseconds":{/i ) {
                warn "begin reached 'test_microseconds'\n";
            }
        }
    }
    open( $self->{'file_out'}, '>', 'docs/test_summary.times.tmp') or die "cannot create docs/test_summary.times.tmp: $!";
    my $parrot_version = qx{./perl6 -e'print %*VM<config><revision>'};
    my $rakudo_version = qx{git log --oneline --max-count=1 .}; chomp $rakudo_version;
    $rakudo_version =~ s/\\/\\\\/g; # escape all backslashes
    $rakudo_version =~ s/\"/\\\"/g; # escape all double quotes
    my $file_out = $self->{'file_out'};
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
        gmtime(time());
    push @test_history, sprintf("[\"%4d-%02d-%02d %02d:%02d:%02d\",%d,\"%s\"]",
        $year+1900, $mon+1, $mday, $hour, $min, $sec,
        $parrot_version, $rakudo_version );
    # Delete the oldest test test_history if there are too many.
    while ( @test_history > $self->{'Timings'} ) { shift @test_history; }
    print $file_out qq!{"test_history":[\n!;
    print $file_out "  " . join(",\n  ",@test_history) . "\n ],\n";
    print $file_out qq! "test_microseconds":{!;
    # tell Test.pm to output per-test timestamps
    $ENV{'PERL6_TEST_TIMES'} = 'true';
    return bless $self;
}

# track simple relative benchmarking
sub log_script_times {
    my $self      = shift;
    my $test_name = shift;
    my $ref_times = shift;
    my $ref_comments = shift;
    my (@times) = @$ref_times;
    my (@comments) = @$ref_comments;
    shift @times;     # offset by 1: the first result becomes $times[0];
    shift @comments;
    for ( my $i=0; $i<=@times; $i++ ) {
        if ( not defined $comments[$i] ) { $comments[$i] = ''; }
        $comments[$i] =~ s/\\/\\\\/g; # escape all backslashes
        $comments[$i] =~ s/\"/\\\"/g; # escape all double quotes
    }
    my ( $line );
    my $file_in  = $self->{'file_in'};
    my $file_out = $self->{'file_out'};
    $test_name =~ s{^t/spec/}{};   # eg 'S02-literals/numeric.t'
    my $test_separator;
    if ( $self->{'Last_test_loaded'} eq '' ) {
        $test_separator = "\n";
    }
    else {
        $test_separator = ",\n";
    }
    while ( not eof($file_in) and $self->{'Last_test_loaded'} lt $test_name ) {
        $line = <$file_in>; chomp $line;
        if ( $line =~ m/^\s\s"(.+)":.$/ ) {
            $self->{'Last_test_loaded'} = $1;
        }
    }
    my @logged_results;
    if ( not eof($file_in) and $self->{'Last_test_loaded'} eq $test_name ) {
        my $line = <$file_in>; chomp $line;
        while ( not eof($file_in) and $line =~ m/^\s\s\s\[(\d+),\[(.+?)\],?/ ) {
            my $test_number = $1;
            my @timings = split /,/ , $2;
            $logged_results[$test_number-1] = [ @timings ];
            $line = <$file_in>; chomp $line;
        }
    }
    my $microseconds = [];
    my $testcount = @times;
    for ( my $test_number=0; $test_number<$testcount; $test_number++) {
        unless ( defined($times[$test_number]) ) { $times[$test_number] = 0; }
        my ( @times_in_file );
        if ( defined @{$logged_results[$test_number]} ) {
            @times_in_file = ( @{$logged_results[$test_number]} );
        }
        push @times_in_file, $times[$test_number];
        if ( not defined( $times_in_file[0] ) ) { shift @times_in_file; }
        # Delete the oldest test timings if there are too many.
        while ( @times_in_file > $self->{'Timings'} ) { shift @times_in_file; }
        $$microseconds[$test_number] = [ @times_in_file ];
    }
    my $test_number = 1; # start from number 1 again
    print $file_out
        $test_separator .
        qq'  "$test_name":[\n' .
        join(",\n", map {'   ['.$test_number++.',['.join(',',@$_).'],"'.$comments[$test_number-2].'"]'} @$microseconds) .
        qq'\n  ]';
}

sub end {
    my $self = shift;
    my $file_in  = $self->{'file_in'};
    my $file_out = $self->{'file_out'};
    print $file_out "\n }\n}\n";
    close $file_out or warn $!;
    close $file_in or warn $!;
    unlink 'docs/test_summary.times';
    rename 'docs/test_summary.times.tmp', 'docs/test_summary.times';
}

package main;

=pod

=head1 NAME

tools/test_summary.pl -- run spectests and make statistical reports

=head1 DESCRIPTION

This test harness written in Perl 5, runs the Perl 6 specification test
suite.  It uses the same Test Anything Protocol (TAP) as for example
L<TAP::Harness>, but does not depend those modules.

The names of the tests are listed in t/spectest.data, or another file
whose name is passed on the command line.

=head2 OUTPUT

The harness prints the name of each test script before running it.
After completion it prints the total number of tests passed, failed,
to do, skipped, and planned.  The descriptions of any tests failed,
skipped or left to do are also listed.

After running all the tests listed, the harness prints a set of
subtotals per Synopsis.

If you set the REV environment variable (with the first 7 characters of
a Rakudo commit id), the harness prints an additional set of grand
totals suitable for adding to F<docs/spectest_progress.csv>.

=head1 SIMPLE RELATIVE BENCHMARKING

Too little information can mislead, hence this self deprecating title.
For example, these measurements overlook variation in test times
('jitter'), kernel versus user process times, and measurement overheads.
But these results are better than no information at all.

If activated, this tool logs the most recent 5 timings in microseconds
in F<docs/test_summary.times> in a specific JSON format, for later
analysis.  Measurement and logging add less than 2% to the testing time
and makes a log file of about 2.5MB.

=head2 Methods

=head3 begin

Accepts an optional parameter, the number of timings per test to keep.
Creates a temporary file for new results, and returns an object that
updates the log file.  (F<begin> acts as the constructor).

=head3 log_script_times

Takes these parameters: test script name, reference to an array of times
in microseconds, reference to an array of test description strings.
Appends the results to the temporary log file.

=head3 end

Closes and renames the temporary log file.

=head2 Timing results file

All results are stored in F<docs/test_summary.times> in a specific JSON
format.  With 35000 test result lines and 5 runs it occupies just under
2.5 MB.

Here is an example with a few semi fictitious results:

    {"test_history":[
      ["2010-05-05 10:15:45",46276,"925629d Make $x does (R1, R2) work."],
      ["2010-05-07 08:58:07",46276,"5713af2 run two more test files"],
      ["2010-05-08 18:08:43",46405,"ab23221 bump PARROT_REVISION"],
      ["2010-05-09 05:53:25",46405,"c49d32b run S04-phasers/rvalues.t"],
      ["2010-05-10 00:44:46",46405,"118f4aa Overhaul sqrt for Numeric / Real."]
     ],
     "test_microseconds":{
      "S02-builtin_data_types/anon_block.rakudo":[
       [1,[6139,7559,6440,6289,5520],"The object is-a 'Sub()'"],
       [2,[6610,6599,6690,6580,6010],"sub { } works"]
      ],
      "S02-builtin_data_types/array.rakudo":[
       [1,[9100,8889,9739,9140,9169],"for 1, 2, 3 does 3 iterations"],
       [2,[5650,5599,6119,9819,5140],"for (1, 2, 3).item 3 iterations"],
       [3,[3920,3770,4190,4410,3350],"for [1, 2, 3] does one iteration"]
      ]
     }
    }

The "test_history" section lists the starting times for all the runs of
F<tools/test_summary.pl> that are recorded in the file.  Then the
"test_microseconds" records show each test filename, possibly fudged,
followed by the test numbers, followed by the times obtained from each
run.  If a test has fewer than the usual number of timings, the timings
will be from the most recent test runs.

The file is read and written by custom code and not a JSON module, to
reduce dependencies.  Altering the file format might cause reading to
fail and could result in data loss.  General purpose JSON parsers should
be able to read the data.  For example the following ranks the tests
from best speedup to worst slowdown.

    #!/usr/bin/perl
    use File::Slurp qw( slurp );
    use JSON;
    my $log_text = slurp('docs/test_summary.times');
    my $log = JSON->new->decode( $log_text );
    # Flatten the data structure to a 2-D array of nonzero test times
    my @timings;
    my $script_hash = $$log{'test_microseconds'};
    for my $script_name ( sort keys %$script_hash ) {
        my $test_list = $$script_hash{$script_name};
        for my $t ( @$test_list ) {
            my $times_count = @{$$t[1]};
            if ( $times_count >= 2 and ${$$t[1]}[$times_count-1] > 0 ) {
                push @timings, [$script_name, $$t[0], $$t[2], ${$$t[1]}[$times_count-2], ${$$t[1]}[$times_count-1] ];
            }
        }
    }
    # Sort the timings into improved/worsened order with a Schwartzian transform
    my @z; for my $t ( @timings ) { push @z, ($$t[4]-$$t[3])/$$t[4]; }
    my @sorted = @timings[ sort { $z[$a] <=> $z[$b] } 0..$#timings ];
    # Display the results: quicker is minus, slower is plus.
    for my $s ( @sorted ) {
        printf "%+3.0f%% %6d %6d %s:%d:%s\n",
           ($$s[4]-$$s[3])*100/$$s[3], $$s[3], $$s[4], $$s[0], $$s[1], $$s[2];
    } # %change, prev-time, latest-time, script, test-num, test-desc

A second example shows another way to read the results file, and ranks
the tests from most to least consistent in execution time.

    #!/usr/bin/perl
    use JSON;
    my $log_text = qx{$^X -MExtUtils::Command -e cat docs/test_summary.times};
    my $log = JSON->new->decode( $log_text );
    # Flatten the data structure to a 2-D array of nonzero test times
    my @timings;
    my $script_hash = $$log{'test_microseconds'};
    for my $script_name ( sort keys %$script_hash ) {
        my $test_list = $$script_hash{$script_name};
        for my $t ( @$test_list ) {
            my $times_count = @{$$t[1]};
            if ( $times_count >= 2 and ${$$t[1]}[$times_count-1] > 0 ) {
                my $min = my $max = ${$$t[1]}[0];
                for my $i (1..$times_count-1) {
                    $min = ${$$t[1]}[$i] if $min > ${$$t[1]}[$i];
                    $max = ${$$t[1]}[$i] if $max < ${$$t[1]}[$i];
                }
                push @timings, [$script_name, $$t[0], $$t[2], $min, $max ] if $min > 0;
            }
        }
    }
    # Sort the timings into most/least consistent order by Schwartzian transform
    my @z; for my $t ( @timings ) { push @z, ($$t[4]-$$t[3])/$$t[3]; }
    my @sorted = @timings[ sort { $z[$a] <=> $z[$b] } 0..$#timings ];
    # Display the results from most to least consistent
    for my $s ( @sorted ) {
        printf "%3.1f%% %6d %6d %s:%d:%s\n",
            ($$s[4]-$$s[3])*100/$$s[3], $$s[3], $$s[4], $$s[0], $$s[1], $$s[2];
    } # %difference, min-time, max-time, script, test-num, test-desc

=head2 TODO

Detect changes in number of tests or descriptions of tests in each
test script, and discard all previous results for that script if there
has been a change.  Consider whether to log total execution time per
test script.

Analyse and report useful results, such as the slowest n tests.

=head1 SEE ALSO

The L<perlperf> module.  The L<http://json.org/> site.

=cut
