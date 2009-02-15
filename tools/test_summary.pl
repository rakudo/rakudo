#! perl

# Copyright (C) 2004-2008, The Perl Foundation.
# $Id$

##  The "make spectest" target tells us how many
##  tests we failed (hopefully zero!), but doesn't say how
##  many were actually passed.  This script runs the spectest
##  tests and summarizes planned, actual, passed, failed, todoed,
##  and skipped test results.
##
##  Usage:
##     tools/test_summary.pl [testlist]
##
##  If supplied, C<testlist> identifies an alternate list of tests
##  to use (e.g., t/localtest.data).

use strict;
use warnings;

my $testlist = $ARGV[0] || 't/spectest.data';

my $fh;
open($fh, '<', $testlist) || die "Can't read $testlist: $!";

my (@tfiles, %tname);
while (<$fh>) {
    /^ *#/ && next;
    my ($specfile) = split ' ', $_;
    next unless $specfile;
    push @tfiles, "t/spec/$specfile";
}
close($fh);

{
    my $cmd = join ' ', $^X, 't/spec/fudgeall', 'rakudo', @tfiles;
    print "$cmd\n";
    @tfiles = split ' ', `$cmd`;
}

@tfiles = sort @tfiles;
my $max = 0;
for my $tfile (@tfiles) {
    my $tname = $tfile; $tname =~ s!^t/spec/!!;
    $tname = substr($tname, 0, 49);
    if (length($tname) > $max) { $max = length($tname); }
    $tname{$tfile} = $tname;
}

my @syn = qw(S02 S03 S04 S05 S06 S09 S10 S12 S13 S16 S17 S29);
my @col = qw(pass fail todo skip plan spec);
my %syn;
my %sum;
for my $syn (@syn) {
    $syn{$syn}++;
    for my $col (@col) {
        $sum{"$syn-$col"} = 0;
    }
}

$| = 1;
printf "%s  pass fail todo skip test plan\n", ' ' x $max;

my @fail;
for my $tfile (@tfiles) {
    my $th;
    open($th, '<', $tfile) || die "Can't read $tfile: $!\n";
    my $plan = 0;
    while (<$th>) {
       if (/^\s*plan\D*(\d+)/) { $plan = $1; last; }
    }
    close($th);
    my $tname = $tname{$tfile};
    my $syn = substr($tname, 0, 3); $syn{$syn}++;
    printf "%s%s..", $tname, '.' x ($max - length($tname));
    my $parrot = -d 'parrot' ? 'parrot/parrot' : '../../parrot';
    my $cmd = "$parrot perl6.pbc $tfile";
    my @results = split "\n", `$cmd`;
    my ($test, $pass, $fail, $todo, $skip) = (0,0,0,0,0);
    my (%skip, %todopass, %todofail);
    for (@results) {
        if    (/^1\.\.(\d+)/) { $plan = $1 if $1 > 0; next; }
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
    my $abort = $plan - $test;
    if ($abort > 0) {
        $fail += $abort;
        push @fail, "$tname aborted $abort test(s)";
        $test += $abort;
    }
    printf "%4d %4d %4d %4d %4d %4d\n",
        $pass, $fail, $todo, $skip, $test, $plan;
    $sum{'pass'} += $pass;  $sum{"$syn-pass"} += $pass;
    $sum{'fail'} += $fail;  $sum{"$syn-fail"} += $fail;
    $sum{'todo'} += $todo;  $sum{"$syn-todo"} += $todo;
    $sum{'skip'} += $skip;  $sum{"$syn-skip"} += $skip;
    $sum{'test'} += $test;  $sum{"$syn-test"} += $test;
    $sum{'plan'} += $plan;  $sum{"$syn-plan"} += $plan;
    for (keys %skip) {
        printf "    %2d skipped: %s\n", $skip{$_}, $_;
    }
    for (keys %todofail) {
        printf "    %2d todo   : %s\n", $todofail{$_}, $_;
    }
    for (keys %todopass) {
        printf "    %2d todo PASSED: %s\n", $todopass{$_}, $_;
    }
}

for my $syn (sort keys %syn) {
    my $ackcmd = "ack plan t/spec/$syn* -wh";
    my @results = `$ackcmd`;
    my $spec = 0;
    for (@results) {
        $spec += $1 if /^\s*plan\s+(\d+)/;
    }
    $sum{"$syn-spec"} = $spec;
    $sum{'spec'} += $spec;
}

my $sumfmt = qq(%-9.9s %6s,%6s,%6s,%6s,%6s,%6s\n);
print "----------------\n";
print qq("Synopsis","pass","fail","todo","skip","regr","spec"\n);
for my $syn (sort keys %syn) {
    printf $sumfmt, qq("$syn",), map { $sum{"$syn-$_"} } @col;
}

my $total = scalar(@tfiles).' regression files';
printf $sumfmt, qq("total",), map { $sum{$_} } @col;

print "----------------\n";
my $rev = $ENV{'REV'};
if ($rev) {
    my $file = scalar(@tfiles);
    print join(',', $rev, (map { $sum{$_} } @col), $file), "\n";
    print "[rakudo]: spectest-progress.csv update: ",
          "$file files, $sum{'pass'} passing, $sum{'fail'} failing\n";
}

if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) { print "    $_\n"; }
}
