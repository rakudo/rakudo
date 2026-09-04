use lib <t/packages/Test-Helpers t/packages/02-rakudo/lib>;
use Test;
use Test::Helpers;
use nqp;

plan 22;

# The language revision belongs to the compilation unit. Compiling another
# unit in the same process, through EVAL or by loading a module that is
# not precompiled, must not change the revision the enclosing unit keeps
# compiling under, and an EVAL starts from the revision of the unit that
# calls it. The unrecognized regex boundary <|x> is an error from 6.e on
# and compiles silently before, so it tells the revisions apart, as does
# the value default of a typed hash, which is Mu from 6.e on and Any
# before.

my $lib = make-temp-dir;
$lib.add('ModC.rakumod').spurt: q:to/CODE/;
    use v6.c;
    unit module ModC;
    sub callee() { CALLER::<$_> }
    our sub outer() { callee() }
    CODE
$lib.add('ModD.rakumod').spurt: q:to/CODE/;
    use v6.d;
    unit module ModD;
    our sub boundary() { EVAL q["x" ~~ / <|x> /; 'compiled'] }
    CODE
$lib.add('ModE.rakumod').spurt: q:to/CODE/;
    use v6.e.PREVIEW;
    unit module ModE;
    our sub revision() { $?LANGUAGE-REVISION }
    CODE

# Compiling a 6.e unit in-process from this 6.d unit
BEGIN CompUnit::Loader.load-source(q:to/SRC/.encode);
    use v6.e.PREVIEW;
    unit module LangRevSixEBegin;
    SRC
my %h{Str};
is %h.of.^name, 'Any',
    'a typed hash declared after a 6.e unit compiled in-process keeps the 6.d value default';
is $?LANGUAGE-REVISION, 2,
    'a unit keeps its own $?LANGUAGE-REVISION after a 6.e unit compiled in-process';
CompUnit::Loader.load-source(q:to/SRC/.encode);
    use v6.e.PREVIEW;
    unit module LangRevSixERuntime;
    SRC
is EVAL(q[$?LANGUAGE-REVISION]), 2,
    'an EVAL starts from the revision of the calling unit after a 6.e unit compiled in-process';
lives-ok { EVAL q["x" ~~ / <|x> /] },
    'an EVAL in a 6.d unit compiles an unrecognized boundary after a 6.e unit compiled in-process';
is Q[$?LANGUAGE-REVISION].AST.EVAL, 2,
    'Str.AST parses under the revision of the calling unit after a 6.e unit compiled in-process';

# Precompiled modules of another revision
use LangRevSixD;
use LangRevSixE;
is LangRevSixE::revision(), 3,
    'an EVAL in a precompiled 6.e module starts from 6.e when called from a 6.d unit';
throws-like { LangRevSixE::boundary() }, X::Syntax::Regex::UnrecognizedBoundary,
    'an EVAL in a precompiled 6.e module rejects an unrecognized boundary when called from a 6.d unit';
is LangRevSixD::boundary(), 'compiled',
    'an EVAL in a precompiled 6.d module compiles an unrecognized boundary';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    use LangRevSixD;
    print LangRevSixD::boundary();
    CODE
    :compiler-args['-It/packages/02-rakudo/lib'],
    :out('compiled'),
    'an EVAL in a precompiled 6.d module starts from 6.d when called from a 6.e unit';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    BEGIN EVAL q[use v6.d; 1];
    "x" ~~ / <|x> /;
    CODE
    :err(/'Unrecognized regex boundary'/), :exitcode(1),
    'an EVAL at BEGIN time that lowers the revision does not lower the rest of the calling unit';

is-run q:to/CODE/,
    use v6.e.PREVIEW;
    EVAL q[use v6.d; 1];
    EVAL q["x" ~~ / <|x> /];
    CODE
    :err(/'Unrecognized regex boundary'/), :exitcode(1),
    'an EVAL that lowers the revision does not lower the revision of later EVALs';

todo 'the legacy frontend does not isolate $/ in a .match call'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is-run q:to/CODE/,
    use v6.e.PREVIEW;
    BEGIN EVAL q[use v6.d; 1];
    "b" ~~ /b/;
    "a".match(/a/);
    print $/;
    CODE
    :out('b'),
    'an EVAL at BEGIN time that lowers the revision does not stop the calling unit isolating $/';

{
    temp %*ENV<RAKUDO_NO_PRECOMPILATION> = 1;

    is-run q:to/CODE/,
        use v6.e.PREVIEW;
        use ModD;
        "x" ~~ / <|x> /;
        CODE
        :compiler-args["-I$lib"],
        :err(/'Unrecognized regex boundary'/), :exitcode(1),
        'compiling a 6.d module in-process keeps the 6.e unit that uses it at 6.e';

    is-run q:to/CODE/,
        use v6.e.PREVIEW;
        use ModD;
        my %h{Str};
        print %h.of.^name;
        CODE
        :compiler-args["-I$lib"],
        :out('Mu'),
        'compiling a 6.d module in-process keeps the 6.e value default of a typed hash';

    is-run q:to/CODE/,
        use ModE;
        "x" ~~ / <|x> /;
        print 'compiled';
        CODE
        :compiler-args["-I$lib"],
        :out('compiled'),
        'compiling a 6.e module in-process keeps the 6.d unit that uses it at 6.d';

    is-run q:to/CODE/,
        use ModE;
        print $?LANGUAGE-REVISION;
        print ModE::revision();
        CODE
        :compiler-args["-I$lib"],
        :out('23'),
        'each unit keeps its own $?LANGUAGE-REVISION after an in-process module compile';

    is-run q:to/CODE/,
        use ModE;
        class Foo { }
        print Foo.HOW.language_revision;
        CODE
        :compiler-args["-I$lib"],
        :out('2'),
        'a class declared after a 6.e module compiled in-process is stamped with the unit revision';

    is-run q:to/CODE/,
        use v6.e.PREVIEW;
        use ModD;
        BEGIN print EVAL q[$?LANGUAGE-REVISION];
        print $?LANGUAGE-REVISION;
        CODE
        :compiler-args["-I$lib"],
        :out('33'),
        'an EVAL at BEGIN time starts from the revision of the calling unit after an in-process module compile';

    is-run q:to/CODE/,
        use v6.e.PREVIEW;
        use ModD;
        Q["x" ~~ / <|x> /].AST;
        CODE
        :compiler-args["-I$lib"],
        :err(/'Unrecognized regex boundary'/), :exitcode(1),
        'Str.AST parses under the revision of the calling unit after an in-process module compile';

    is-run q:to/CODE/,
        use v6.e.PREVIEW;
        use ModC;
        $_ = 'top';
        print ModC::outer().raku;
        CODE
        :compiler-args["-I$lib"],
        :out('Any'),
        'a 6.c module compiled in-process keeps its dynamic topic when used from a 6.e unit';

    is-run q:to/CODE/,
        use v6.c;
        use ModD;
        sub callee() { CALLER::<$_> }
        sub outer() { callee() }
        $_ = 'top';
        print outer().raku;
        CODE
        :compiler-args["-I$lib"],
        :out('Any'),
        'a 6.c unit keeps its dynamic topic after a 6.d module compiled in-process';

    is-run q:to/CODE/,
        use v6.c;
        use ModE;
        my \type := Metamodel::ClassHOW.new_type(:name<Rt>);
        type.^compose;
        print type.HOW.language_revision;
        CODE
        :compiler-args["-I$lib"],
        :out('1'),
        'a class composed at runtime is stamped with the revision of the unit composing it';
}

# vim: expandtab shiftwidth=4
