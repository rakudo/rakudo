use lib <t/02-rakudo/test-packages>;
use Test;
use MONKEY-SEE-NO-EVAL;
use nqp;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 3;

ok EVAL('use v5-dashed; dashed-module-loaded()'),
  'a leading `use` of a module whose name starts with a version-like part loads the module';

is EVAL('use v6; v1.2+.raku'), 'v1.2+',
  'a version may still be given a + adverb';

if $rakuast {
    is EVAL('use v6; v1.2-.raku'), 'v1.2-',
      'a version may still be given a - adverb';
}
else {
    skip 'the - version adverb is only supported by the RakuAST frontend';
}

# vim: expandtab shiftwidth=4
