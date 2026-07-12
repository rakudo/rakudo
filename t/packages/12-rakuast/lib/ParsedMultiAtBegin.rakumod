# Precompiling this module used to fail: the multi's implicit proto is a
# generated declaration on the Str.AST parse unit's mainline scope, which
# was lost when EVAL wrapped the already-begun statements in a fresh unit.
use MONKEY-SEE-NO-EVAL;
unit module ParsedMultiAtBegin;
my $c = BEGIN 'multi keep(Int \v) { v + 1 }; &keep'.AST.EVAL;
our sub bump($n) { $c($n) }
