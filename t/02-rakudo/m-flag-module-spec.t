use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

# A -M argument is parsed as the module part of a use statement, so it
# takes the same adverbs and import lists as a use statement in source.

plan 19;

my @fixture = '-I', 't/packages/02-rakudo/m-flag-fixture';

is-run 'say fixture-loaded',
    'a plain -M loads the module and imports its default exports',
    :compiler-args[|@fixture, '-M', 'MFlag::Fixture'], :out("MFlag::Fixture loaded\n");

is-run 'say 1 mflag 2',
    'an operator exported by a -M module parses in the program',
    :compiler-args[|@fixture, '-M', 'MFlag::Fixture'], :out("1 mflag 2\n");

is-run 'say EVAL "1 + 1"',
    'a pragma given with -M applies to the program',
    :compiler-args['-M', 'MONKEY-SEE-NO-EVAL'], :out("2\n");

is-run 'declarer-class Foo { method m { "declared" } }; say Foo.m',
    'a declarator exported through EXPORTHOW by a -M module parses in the program',
    :compiler-args['-I', 't/packages/02-rakudo/lib', '-M', 'ExporthowDeclarer'],
    :out("declared\n");

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is-run 'say fixture-loaded',
        'a -M with matching :ver, :auth and :api adverbs loads the module',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:ver<1.*>:auth<test:fixture>:api<2>'],
        :out("MFlag::Fixture loaded\n");

    is-run 'say fixture-loaded',
        'a -M with a :ver<1.+> adverb loads the module',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:ver<1.+>'],
        :out("MFlag::Fixture loaded\n");

    is-run 'say fixture-loaded',
        'a -M with a :ver adverb that matches no installed version fails to compile',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:ver<2.*>'],
        :err(/'Could not find MFlag::Fixture:ver<2.*>'/), :exitcode(1);

    is-run 'say fixture-loaded',
        'a -M with a :auth adverb that matches nothing fails to compile',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:auth<nobody>'],
        :err(/'Could not find MFlag::Fixture:auth<nobody>'/), :exitcode(1);

    is-run 'say fixture-tagged',
        'a -M with an import tag imports that tag',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture :tagged'],
        :out("MFlag::Fixture tagged\n");

    is-run 'say fixture-loaded',
        'a -M with an import tag does not import the default exports',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture :tagged'],
        :err(/'Undeclared routine'/), :exitcode(1);

    is-run 'say fixture-loaded() ~ " " ~ EVAL q/"and evaluated"/',
        'two -M arguments both apply',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture', '-M', 'MONKEY-SEE-NO-EVAL'],
        :out("MFlag::Fixture loaded and evaluated\n");

    is-run 'say 1',
        'a -M with trailing garbage is reported as malformed, located in the argument',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture)'],
        :err(/'Malformed -M argument' \s+ 'at -M:1' .* 'use MFlag::Fixture'/), :exitcode(1);

    is-run 'say 1',
        'an empty -M is reported as malformed',
        :compiler-args['-M', ''],
        :err(/'Malformed -M argument'/), :exitcode(1);

    is-run 'say fixture-tagged',
        'whitespace around a -M argument is ignored',
        :compiler-args[|@fixture, '-M', ' MFlag::Fixture :tagged '],
        :out("MFlag::Fixture tagged\n");

    is-run "say 1;\nEND\nsay 2",
        'a heredoc started in a -M argument does not take its body from the program',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture :tagged, q:to/END/'],
        :err(/'Ending delimiter END not found'/), :exitcode(1);

    is-run "my \$a = 1;\nmy \$b = 1;\nmy \$c = 1 1;",
        'a quote in a -M argument does not turn into a runaway quote hint for the program',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:ver<1.*>'],
        :err(/'Two terms in a row' <!before ' (runaway'>/), :exitcode(1);

    is-run 'say 1',
        'a compile time error inside a -M argument names -M as its source',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture :nosuchtag'],
        :err(/'nosuchtag' .* 'at -M:1'/), :exitcode(1);

    is-run 'say 1',
        'a -M with an unterminated adverb is a compile error',
        :compiler-args[|@fixture, '-M', 'MFlag::Fixture:ver<1.*'],
        :err(/"couldn't find final '>'"/), :exitcode(1);

    is-run 'say 1',
        'a -M that is only a version is rejected like use v6 would be',
        :compiler-args['-M', 'v6.d'],
        :err(/'Too late'/), :exitcode(1);
}
else {
    skip-rest 'adverbs on -M are only supported by the RakuAST frontend';
}
