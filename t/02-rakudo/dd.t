use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 2;

is-run 'dd Failure.new',
  :err(｢Failure.new(exception => X::AdHoc.new(payload => "Failed"))｣ ~ "\n"),
  ｢Failures don't get marked as handled in &dd｣;

is-run 'dd 42.any', :err("any(42)\n"), 'Junctions do not crash';

# vim: expandtab shiftwidth=4
