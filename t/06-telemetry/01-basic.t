use v6;
use lib <t/packages/>;
use Test;
use Test::Helpers;

# very basic Telemetry tests

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

plan 42;

my $T = T;
isa-ok $T, Telemetry, 'did we get a Telemetry object from T';
for <wallclock cpu max-rss> {
    ok $T{$_}, "did we get a non-zero value for $_ using AT-KEY";
    ok $T."$_"(), "did we get a non-zero value for $_ with a method";
}

my $T2 = $T.raku.EVAL;
isa-ok $T2, Telemetry, 'did we get a Telemetry object from T.raku.EVAL';
is $T2{$_}, $T{$_}, "did $_ roundtrip ok in Telemetry?"
  for <wallclock cpu max-rss>;

my $P = T() - $T;
isa-ok $P, Telemetry::Period, 'Did we get a Telemetry::Period';
for <wallclock cpu> {
    ok $P{$_}, "did we get a non-zero value for $_ using AT-KEY";
    ok $P."$_"(), "did we get a non-zero value for $_ using AT-KEY";
}

my $P2 = $P.raku.EVAL;
isa-ok $P2, Telemetry::Period,
  'did we get a Telemetry::Period object from period.raku.EVAL';
is $P2{$_}, $P{$_}, "did $_ roundtrip ok in Telemetry::Period?"
  for <wallclock cpu max-rss>;

my $sampler = $T.sampler;
isa-ok $sampler, Telemetry::Sampler, 'did it contain a Sampler';

my @instruments = $sampler.instruments;
is +@instruments, 2, 'there are 2 default default instruments';
for (Telemetry::Instrument::Usage, Telemetry::Instrument::ThreadPool).kv
  -> $index, $class {
    isa-ok @instruments[$index], $class, "did we get a $class.^name()";
}

for <&snap &snapper &periods &report &safe-ctrl-c &T> -> $name {
    isa-ok ::{$name}, Sub, "was $name exported";
}

is snap, Nil, 'did the snap return nothing';
my @periods = periods;
is +@periods, 1, 'did periods auto-add an extra snap?';
isa-ok @periods[0], Telemetry::Period, 'is it a Telemetry::Period';

is +periods, 0, 'Did the call to periods remove all of the snaps?';

is snapper, Nil, 'did the snapper return nothing';
sleep 1;
snapper(:stop);
sleep .1;

ok +periods() > 0, 'did the snapper start taking snaps';
sleep .2;
ok +periods() == 0, 'did the snapper actually stop';

snapper(2);
sleep .5;  # give snapper thread some time to start up
is +periods(), 1, 'did the snapper start taking snaps';
snapper(:stop);
sleep 2;

my @report = report.lines;
is +@report, 2, 'did we only get the header of the report';
ok @report[0].starts-with('Telemetry Report of Process'), 'line 1 of report';
is @report[1], 'Number of Snapshots: 0', 'line 2 of report';

{ # https://github.com/rakudo/rakudo/issues/1714
    (temp %*ENV)<RAKUDO_REPORT_COLUMNS> = 'blahblah';
    is-run ｢
      use snapper;
      for ^1_000 {
          (^100).race(batch=>1).map({ $_ }).List
      };
      print 'pass'
    ｣, :err{.contains: 'Unknown Telemetry column `blahblah`'}, :out<pass>,
    'giving unknown column in RAKUDO_REPORT_COLUMNS warns instead of crashing'
}

# vim: expandtab shiftwidth=4
