use lib <t/02-rakudo/test-packages>;
use MONKEY-SEE-NO-EVAL;
use Test;
use EvalMainlineGlobal;

plan 5;

lives-ok { EVAL q{use MONKEY-SEE-NO-EVAL; BEGIN EVAL q{sub exported-at-begin is export { }}} },
  'an EVAL at BEGIN time can export a sub';
lives-ok { EVAL q{use MONKEY-SEE-NO-EVAL; class C { BEGIN EVAL q{our sub exported-in-class is export { }} }} },
  'an EVAL at BEGIN time inside a class can export a sub';
nok GLOBAL::<EXPORT>:exists,
  'exporting from an EVAL at BEGIN time makes no GLOBAL::EXPORT';
is eval-mainline-found(), 'found',
  'an EVAL in the mainline of a precompiled module finds a class of that module';
is-deeply (try EVAL q{use MONKEY-SEE-NO-EVAL; class EvalInClass { BEGIN EVAL q{sub in-class-export is export { }} }; EvalInClass.WHO<EXPORT>:exists}) // 'died',
  False,
  'an EVAL at BEGIN time inside a class exports nothing into that class';
