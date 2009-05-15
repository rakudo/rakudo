#!/usr/bin/perl
use v5;
use Time::HiRes qw( gettimeofday tv_interval );

my %benchmarks = (
# The Hello World benchmark mostly exists to benchmark our start-up time.
"01 - hello world" => q{;
    "Hello, world!"; # don't say, don't want output from benchmarks
},

# This one tests our performance at calling a single-dispatch sub (and the
# cost of signature binding somewhat). Note loop of 5,000 and 2 calls so it
# matches the other tests in all other respects.
"02 - 10,000 sub dispatches" => q{
    sub foo(Int $x, Int $y) { }
    for 1..5000 {
        foo(1,2);
        foo(1,2);
    }
},

# This one tests our performance for calling multi-dispatch subs.
"03 - 10,000 multi dispatches" => q{
    multi foo(Int $x, Str $y) { }
    multi foo(Str $x, Int $y) { }
    for 1..5000 {
        foo(1, "hi");
        foo("hi", 1);
    }
},

# This one tests our performance for method dispatch.
"04 - 10,000 method dispatches" => q{
    class A { method m1(Int $x, Int $y) { }; method m2(Int $x, Int $y) { } }
    my $x = A.new;
    for 1..5000 {
        $x.m1(1,2);
        $x.m2(1,2);
    }
},

# This one is for multi-method dispatch.
"05 - 10,000 multi-method dispatches" => q{
    class A {
        multi method m(Int $x, Str $y) { }
        multi method m (Str $x, Int $y) { }
    }
    my $x = A.new;
    for 1..5000 {
        $x.m(1, "hi");
        $x.m("hi", 1);
    }
},

);

# Run the benchmarks and output results.
for (sort keys %benchmarks) {
    print "$_: ";
    open my $fh, "> bm_current.p6";
    print $fh $benchmarks{$_};
    close $fh;
    my $start = [gettimeofday];
    system('perl6 bm_current.p6') && die 'Error running benchmark';
    print tv_interval($start) . "\n";
}
