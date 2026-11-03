use v6.e.PREVIEW;
use lib <t/packages/Test-Helpers>;
use nqp;
use Test;
use Test::Helpers;
use MONKEY-SEE-NO-EVAL;

plan 7;

# Format.new used to bind a Failure to its directives attribute: the
# @*DIRECTIVES dynamic it read was only declared inside Formatter.AST.
# That broke .directives and made instances unserializable, since the
# Failure's backtrace references live compiler frames.
is-deeply Format.new('%5d:%3x').directives, ("d", "x"),
    'Format.new populates .directives';
is-deeply Format.new('%5d:%3x').directives, ("d", "x"),
    '.directives is also populated on a Formatter cache hit';

# The format string quote only exists in the RakuAST grammar.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    my $f = EVAL 'use v6.e.PREVIEW; q:o/%5x/';
    isa-ok $f, Format, 'q:o produces a Format object';
    is $f(255), '   ff', 'the format renders its argument';
    is-deeply $f.directives, ("x",), 'the literal Format knows its directives';

    # A constant format literal is built at compile time and serialized,
    # so it must survive a precompilation round trip.
    my $dir = make-temp-dir();
    $dir.add('FmtLit.rakumod').spurt: q:to/CODE/;
        use v6.e.PREVIEW;
        unit module FmtLit;
        my $f = q:o/%5d/;
        our sub render($n) { $f($n) }
        our sub directives() { $f.directives }
        CODE
    for 'compiles', 'loads from the precompilation store' -> $stage {
        is-run 'use FmtLit; print FmtLit::render(42) ~ "|" ~ FmtLit::directives()',
            :compiler-args['-I', $dir.absolute],
            :out("   42|d"),
            :err(""),
            "a module with a constant format literal $stage";
    }
}
else {
    skip 'format string literals require the RakuAST frontend', 5;
}

# vim: expandtab shiftwidth=4
