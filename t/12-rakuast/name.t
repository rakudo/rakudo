use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

given RakuAST::Name.from-identifier('foo') {
    isa-ok $_, RakuAST::Name, '.from-identifier constructs a name';
    ok .is-identifier, 'The element is considered an identifier';
    is-deeply .parts.elems, 1, 'Has one part';
    isa-ok .parts[0], RakuAST::Name::Part::Simple, 'Name has a single part';
    is-deeply .parts[0].name, 'foo', 'Part has expected name';
    is-deeply .DEPARSE, 'foo', 'Deparses in an expected way';
}
