# Precompiling this module used to fail with "missing static code ref for
# closure": the Sub was stubbed during the Str.AST parse, whose context is
# abandoned after the parse, so the BEGIN-time EVAL's compilation never
# registered the code object or cleared its compiler stub state.
use MONKEY-SEE-NO-EVAL;
unit module ParsedSubAtBegin;
my $c = BEGIN 'sub ($x) { $x + 1 }'.AST.EVAL;
our sub bump($n) { $c($n) }
