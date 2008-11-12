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

$| = 1;
printf "%s  plan test pass fail todo skip\n", ' ' x $max;
my %sum;
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
    printf "%s%s..%4d", $tname, '.' x ($max - length($tname)), $plan;
    my $cmd = "../../parrot perl6.pbc $tfile";
    my @results = split "\n", `$cmd`;
    my ($test, $pass, $fail, $todo, $skip) = (0,0,0,0,0);
    my (%skip, %todopass, %todofail);
    for (@results) {
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
    printf " %4d %4d %4d %4d %4d\n", $test, $pass, $fail, $todo, $skip;
    $sum{'plan'} += $plan;
    $sum{'test'} += $test;
    $sum{'pass'} += $pass;
    $sum{'fail'} += $fail;
    $sum{'todo'} += $todo;
    $sum{'skip'} += $skip;
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

print "----------------\n";
if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) { print "    $_\n"; }
}
my $total = scalar(@tfiles)." test files";
$total .= ' ' x ($max-length($total));
printf "%s  %4d %4d %4d %4d %4d %4d\n",
    $total, $sum{'plan'}, $sum{'test'}, $sum{'pass'},
    $sum{'fail'}, $sum{'todo'}, $sum{'skip'};
