use v6;

# extensive tests for Telemetry::Instrument::ThreadPool

# make sure we don't have any overrides
BEGIN {
     %*ENV.DELETE-KEY($_) for <
      RAKUDO_REPORT_COLUMNS
      RAKUDO_REPORT_HEADER_REPEAT
      RAKUDO_REPORT_LEGEND
      RAKUDO_REPORT_CSV
      RAKUDO_TELEMETRY_INSTRUMENTS
    >;
}

use Test;
use Telemetry;

plan 104;

# Check ways to create the T:I:ThreadPool instrument
for 'ThreadPool', Telemetry::Instrument::ThreadPool -> \instrument {
    my $what = instrument.^name;
    is $*SAMPLER.set-instruments(instrument), Nil,
      "return Nil when setting with $what";
    my @instruments = $*SAMPLER.instruments;
    is +@instruments, 1,
      "Was one instrument set with $what";
    is @instruments[0], Telemetry::Instrument::ThreadPool,
      "do we get the right class when setting with $what";
}

# Get the column names in alphabetic order
my $instrument = $*SAMPLER.instruments[0];
my @columns = $instrument.columns;
ok ([le] @columns), "are the columns in alphabetical order";

# Assume T:I:ThreadPool is only instrument from now on
my $snap1 = $instrument.snap;
my $snap-class = $snap1.WHAT;
isa-ok $snap-class, Telemetry::Instrument::ThreadPool::Snap;

# Test snap roundtripping
my $snap2 = $snap1.raku.EVAL;
isa-ok $snap2, $snap-class, "Did we get a {$_.^name} after roundtripping";
for @columns {
    is $snap2{$_}, $snap1{$_}, "did we get the same value for $_";
}

# Initialize first set of values.  The left-shift is needed because the x-rss
# columns are right-shifted 10 before being returned on MacOS, because MacOS
# returns the value in bytes rather than Kbytes.
my @values1 = (1..10).map: * +< 10;

# Initialize second set of values.  Use higher values than in first set so
# that we can guarantee a positive difference.
my @values2 = (101..110).map: * +< 10;

# Create a repeatable snap/telemetry for testing
my $S1 = $snap-class.new( |@values1 );
isa-ok $S1, $snap-class, 'did we get a Snap object from first set of values';
my $T1 = Telemetry.new($S1);
isa-ok $T1, Telemetry, 'did we get a Telemetry object from $S1';

# Test all columns for sanity, we don't know which value winds up in which column
for @columns {
    ok $T1{$_}:exists, "does $_ exist in $T1.^name()?";
    diag $T1{$_}.raku unless
      ok $T1{$_}, "did we get a non-zero value for $_ using AT-KEY";
    diag $T1."$_"().raku unless
      ok $T1."$_"(), "did we get a non-zero value for $_ with a method";
    is $T1{$_}, $T1."$_"(), 'did AT-KEY and method on T give the same value';
}

# Create another repeatable snap/telemetry for testing.  Use higher values than
# before so that we can guarantee a positive difference.
my $S2 = $snap-class.new( |@values2 );
isa-ok $S2, $snap-class, 'did we get a Snap object from second set of values';
my $T2 = Telemetry.new($S2);
isa-ok $T2, Telemetry, 'did we get a Telemetry object from $S2';

# Create a repeatable period and check for resulting values
my $P1 = $T2 - $T1;
isa-ok $P1, Telemetry::Period, 'Did we get a T::Period';
for @columns {
    ok $P1{$_}:exists, "does $_ exist in $P1.^name()?";
    ok $P1{$_} > 0, "did we get a positive value for $_ using AT-KEY";
    ok $P1."$_"() > 0, "did we get a positive value for $_ using AT-KEY";
    is $P1{$_}, $P1."$_"(), "did AT-KEY/method on T:P give same value for $_";
}

# vim: expandtab shiftwidth=4
