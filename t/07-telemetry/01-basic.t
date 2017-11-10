use v6;

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

plan 24;

my $T = T;
isa-ok $T, Telemetry, 'did we get a Telemetry object from T';
ok $T{$_}, "did we get a non-zero value for $_"
  for <wallclock cpu max-rss>;

my $period = T() - $T;
isa-ok $period, Telemetry::Period, 'Did we get a Telemetry::Period';
ok $period{$_}, "did we get a non-zero value for $_"
  for <wallclock cpu>;

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
