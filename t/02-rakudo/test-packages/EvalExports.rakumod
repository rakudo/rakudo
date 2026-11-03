use MONKEY-SEE-NO-EVAL;

BEGIN EVAL q{sub exported-by-begin-eval is export { 'begin' }};
EVAL q{sub exported-by-mainline-eval is export { 'mainline' }};
