use nqp;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 1;

# $=rakudoc is only populated by the RakuAST frontend; skip elsewhere.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    # Docs declared inside a `unit class` must reach $=rakudoc, the same as a
    # top-level declaration. The collection used to skip package bodies, so
    # $=rakudoc came out empty for unit-scoped programs (and --rakudoc printed
    # nothing).
    my @blocks = EVAL q:to/PROGRAM/;
        unit class Things;
        #| Process a thing.
        method thing(
            Str $foo #= the foo
        ) { }
        $=rakudoc
        PROGRAM
    ok @blocks.elems > 0,
        '$=rakudoc collects docs declared inside a unit class';
}
else {
    skip '$=rakudoc requires the RakuAST frontend';
}

# vim: expandtab shiftwidth=4
