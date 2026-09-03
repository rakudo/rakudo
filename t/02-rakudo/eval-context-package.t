use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# An EVAL takes $?PACKAGE from its context when that declares one. A
# setting context such as CORE:: declares none, and an EVAL run outside
# any compilation then starts in GLOBAL, where its declarations land.

plan 8;

ok (EVAL Q[$?PACKAGE], :context(CORE::)) === GLOBAL,
  '$?PACKAGE under the CORE:: context is GLOBAL';

ok (EVAL Q[::?PACKAGE], :context(CORE::)) === GLOBAL,
  '::?PACKAGE under the CORE:: context is GLOBAL';

EVAL Q[our sub from-core-eval() { $?PACKAGE }], :context(CORE::);
ok GLOBAL::<&from-core-eval>() === GLOBAL,
  'an our sub under the CORE:: context lands in the package $?PACKAGE names';

my $inner = EVAL Q[class Inner { method pkg { $?PACKAGE } }; Inner], :context(CORE::);
ok $inner.pkg === $inner,
  '$?PACKAGE inside a class declared in the unit is that class';

class Outer {
    our sub pkg { EVAL Q[$?PACKAGE], :context(CORE::) }
}
ok Outer::pkg() === GLOBAL,
  'at runtime the context decides the package, not the package of the caller';

ok (EVAL RakuAST::Var::Compiler::Lookup.new('$?PACKAGE'), :context(CORE::)) === GLOBAL,
  '$?PACKAGE in an AST handed to EVAL under the CORE:: context is GLOBAL';

is-deeply EVAL(Q[package Nested { ::?PACKAGE }, ::?PACKAGE], :context(CORE::)).map(*.^name).List,
  ('Nested', 'GLOBAL'), 'a package declared in the unit has its own ::?PACKAGE';

sub peek { EVAL Q[$x], :context(CALLER::) }
my $x = 'seen';
is peek(), 'seen', 'a runtime EVAL with the CALLER:: context sees a lexical of the caller';

# vim: expandtab shiftwidth=4
