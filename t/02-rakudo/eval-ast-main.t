use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# A MAIN declared in EVALed code is not the program's entry point, so EVAL
# declares it without running it against @*ARGS. @*ARGS below matches its
# signature, so a MAIN run by mistake records its argument instead of exiting.

plan 12;

my @*ARGS = 'from-args';
my $*MAIN-RAN;

my $source = Q|sub MAIN($x) { $*MAIN-RAN = $x }; 42|;

$*MAIN-RAN = Nil;
is $source.AST.EVAL, 42,
  'EVAL of the statement list from Cool.AST returns the mainline value';
nok $*MAIN-RAN.defined,
  'EVAL of the statement list from Cool.AST does not run its MAIN';

$*MAIN-RAN = Nil;
is EVAL($source.AST(:compunit)), 42,
  'EVAL of the comp unit from Cool.AST returns the mainline value';
nok $*MAIN-RAN.defined,
  'EVAL of the comp unit from Cool.AST does not run its MAIN';

$*MAIN-RAN = Nil;
is EVAL(RakuAST::StatementList.new(|$source.AST.statements)), 42,
  'EVAL of a statement list built from parsed statements returns the mainline value';
nok $*MAIN-RAN.defined,
  'EVAL of a statement list built from parsed statements does not run its MAIN';

$*MAIN-RAN = Nil;
my $main-statement = Q|sub MAIN($x) { $*MAIN-RAN = $x }|.AST.statements.head;
is EVAL($main-statement).name, 'MAIN',
  'EVAL of a single statement declaring MAIN returns the sub';
nok $*MAIN-RAN.defined,
  'EVAL of a single statement declaring MAIN does not run it';

$*MAIN-RAN = Nil;
is EVAL($source.AST(:expression)).name, 'MAIN',
  'EVAL of a sub expression declaring MAIN returns the sub';
nok $*MAIN-RAN.defined,
  'EVAL of a sub expression declaring MAIN does not run it';

$*MAIN-RAN = Nil;
is EVAL($source), 42,
  'EVAL of a string declaring MAIN returns the mainline value';
nok $*MAIN-RAN.defined,
  'EVAL of a string declaring MAIN does not run it';
