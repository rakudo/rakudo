use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 18;

# Compile-time validation of `==>` / `<==` stages, matching the
# legacy frontend's `make_feed` in src/Perl6/Actions.nqp.

# --- Invalid shapes that must SORRY ---

is-run q|5 ==> 10; say "ok"|,
    'literal ==> literal SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|5 <== 10; say "ok"|,
    'literal <== literal SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|my $x = grep(* > 4) <== map(* * 2) <== (1..5); say $x|,
    'my $x = ... <== ... <== source SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|my @x = grep(* > 4) <== map(* * 2) <== (1..5); say @x|,
    'my @x = ... <== ... <== source SORRYs with shaped-array wording',
    :err(/'Cannot feed into shaped arrays'/),
    :exitcode(1);

is-run q|our @x = grep(* > 4) <== map(* * 2) <== (1..5); say @x|,
    'our @x = ... <== ... <== source SORRYs with shaped-array wording',
    :err(/'Cannot feed into shaped arrays'/),
    :exitcode(1);

is-run q|sub f { state @x = grep(* > 4) <== map(* * 2) <== 1..3; @x }; say f|,
    'state @x = ... gets the generic SORRY, NOT shaped-array wording',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|sub double($x) { $x * 2 }; my @a = (1..3) ==> &double; say @a|,
    '&lexical-code-object as stage SORRYs with sink-into-code-object wording',
    :err(/'A feed may not sink values into a code object'/),
    :exitcode(1);

# `&Pkg::foo`: rakuast gives the helpful "code object" hint; legacy
# gives the generic message.  Match either.
is-run q|class C { our sub foo($x) { $x * 2 } }; (1..3) ==> &C::foo; say "ok"|,
    '&Pkg::foo (Var::Package with & sigil) SORRYs',
    :err(/'A feed may not sink values into a code object' | 'Only routine calls or variables'/),
    :exitcode(1);

is-run q|say ((1..3) ==> [].push)|,
    'method call ([].push) on RHS of feed SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|my @a; (1..3) ==> @a.append; say @a|,
    'method call (@a.append) on RHS of feed SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

is-run q|say ((1..5) ==> map(* * 2) ==> .grep(* > 4))|,
    'bare .method call as feed stage SORRYs',
    :err(/'Only routine calls or variables'/),
    :exitcode(1);

# --- Valid feeds (no error) ---

is-run q|say ((1..5) ==> map(* * 2) ==> grep(* > 4))|,
    'forward feed chain returns expected value',
    :out("(6 8 10)\n");

is-run q|say (grep(* > 4) <== map(* * 2) <== (1..5))|,
    'backward feed chain returns expected value',
    :out("(6 8 10)\n");

is-run q|my @x; (1..5) ==> map(* * 2) ==> @x; say @x|,
    'forward feed into @-var',
    :out("[2 4 6 8 10]\n");

is-run q|my @x; @x <== map(* * 2) <== (1..5); say @x|,
    'backward feed into @-var',
    :out("[2 4 6 8 10]\n");

is-run q|my $x = (1..5); say ($x ==> map(* * 2))|,
    'scalar var source feeds to a call',
    :out("(2..10)\n");

is-run q|say ((1..3) ==> map(* + 10))|,
    '2-stage forward feed: source ==> call',
    :out("(11 12 13)\n");

# Bare `my @a` (no initializer) is treated as the destination Var,
# not as an assignment expression.  Matches legacy and the
# S03-feeds/basic.t shape `my @a <== gather ...`.
is-run q|my @a <== gather for 1..3 -> $i { take $i }; say @a|,
    'bare my @a <== source compiles (declaration acts as Var)',
    :out("[1 2 3]\n");

# vim: expandtab shiftwidth=4
