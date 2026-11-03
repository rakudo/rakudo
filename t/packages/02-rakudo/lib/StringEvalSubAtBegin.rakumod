use MONKEY-SEE-NO-EVAL;
# Pins ForeignCode's string-EVAL path through IMPL-FIXUP-COMPILED-CODEREFS:
# a BEGIN-time EVAL Sub must survive its outer module's precomp.
our &foo;
BEGIN { &foo = EVAL 'sub () { "from-precomped-string-eval" }' }
