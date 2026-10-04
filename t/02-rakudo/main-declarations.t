use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

plan 8;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

# https://github.com/rakudo/rakudo/issues/6794
is-run 'constant &MAIN = sub { print "ran" }',
  :out<ran>,
  'a constant &MAIN is run as MAIN';

is-run 'my constant &MAIN = sub { print "ran" }',
  :out<ran>,
  'a my constant &MAIN is run as MAIN';

is-run 'constant &MAIN = sub ($x) { print "ran with $x" }',
  :args['foo'],
  :out('ran with foo'),
  'a constant &MAIN receives its command line arguments';

is-run 'constant &MAIN = sub ($x) { print "ran with $x" }',
  :err(/^Usage/),
  :exitcode(2),
  'a constant &MAIN produces a usage message when its arguments are missing';

is-run 'my &MAIN = sub { print "ran" }',
  :out<ran>,
  'a sub assigned to &MAIN is run as MAIN';

if $rakuast {
    is-run 'my &MAIN := sub { print "ran" }',
      :out<ran>,
      'a sub bound to a my &MAIN is run as MAIN';

    is-run 'our &MAIN := sub { print "ran" }',
      :out<ran>,
      'a sub bound to an our &MAIN is run as MAIN';

    is-run 'state &MAIN = sub { print "ran" }',
      :out<ran>,
      'a sub assigned to a state &MAIN is run as MAIN';
}
else {
    skip 'legacy passes the unbound &MAIN type object to RUN-MAIN', 3;
}

# vim: expandtab shiftwidth=4
