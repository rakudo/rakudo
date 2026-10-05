use MONKEY-SEE-NO-EVAL;

sub setup { class EvalMainlineInBlock { method hi { 'found' } } }
my $found = (try EVAL q{EvalMainlineInBlock.hi}) // 'not found';

sub eval-mainline-found() is export { $found }
