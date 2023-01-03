use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

given RakuAST::Label.new('Foo') {
    isa-ok $_, RakuAST::Label, '.new constructs a label';
    is .lexical-name, "Foo", 'is the name correct';
    is .default-scope, 'my', 'is default scope ok';
    is-deeply .allowed-scopes, ('my',), 'are allowed-scopes ok';
    is-deeply .DEPARSE, 'Foo:', 'Deparses in an expected way';
}

# vim: expandtab shiftwidth=4
