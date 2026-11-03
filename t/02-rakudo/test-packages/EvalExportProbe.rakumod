unit module EvalExportProbe;
use MONKEY-SEE-NO-EVAL;

our sub own-export() { EXPORT }
our sub eval-export() { EVAL q{EXPORT} }
