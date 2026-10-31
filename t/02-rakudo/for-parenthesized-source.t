use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 33;

my $rakuast = nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is EVAL(q[my @seen; for (my $n = 1)..3 { @seen.push($_) }; "@seen[] $n"]), '1 2 3 1',
    'a variable declared at the start of a parenthesized for source is initialized';
is EVAL(q[for (my $n := 1)..3 { }; $n]), 1,
    'a variable bound at the start of a parenthesized for source is bound';
is EVAL(q[my @seen; for (my $n = 5) { @seen.push($_) }; "@seen[] $n"]), '5 5',
    'a for over a lone parenthesized declaration iterates its value';
is EVAL(q[my @seen; for (my $n = 1), 2 { @seen.push($_) }; "@seen[] $n"]), '1 2 1',
    'a variable declared in the first of a list of for sources is initialized';
is EVAL(q[sub f { for (my $n = 1)..3 { }; $n }; f()]), 1,
    'a variable declared in a parenthesized for source in a routine is initialized';
is EVAL(q[my @c; for (my $n = 1)..3 { @c.push({ $n }) }; @c.map({ .() }).join]), '111',
    'a block in the loop body closes over a variable declared in the for source';
if $rakuast {
    lives-ok { EVAL q[use fatal; for (my $n = 1)..3 { }] },
        'a variable declared in a parenthesized for source is declared once';
}
else {
    skip 'the legacy frontend declares the variable twice', 1;
}

my @begun;
EVAL q[for (BEGIN @begun.push(1)), 2 { }];
is +@begun, 1, 'a BEGIN in a parenthesized for source runs once';

if $rakuast {
    is EVAL(q[for (sub foo { 7 }, 2) { }; foo()]), 7,
        'a sub declared in a parenthesized for source is declared once';
    is EVAL(q[for (my class Foo { }, 1) { }; Foo.^name]), 'Foo',
        'a class declared in a parenthesized for source is declared once';
    is EVAL(q[for (my enum E <a b>), 2 { }; b.value]), 1,
        'an enum declared in a parenthesized for source is declared once';
}
else {
    skip 'the legacy frontend dies with a redeclaration', 3;
}

if $rakuast {
    is EVAL(q:to/CODE/), 'a,b',
        my @seen;
        for (q:to/END/).lines { @seen.push($_) }
            a
            b
            END
        @seen.join(',')
        CODE
        'a heredoc in a parenthesized for source is read once';
}
else {
    skip 'the legacy frontend reads the heredoc twice', 1;
}
is ((try EVAL q[for (say), 2 { }]) // $!).^name, 'X::Obsolete',
    'a compile error in a parenthesized for source is reported once';

throws-like q[for (my $i = 0; $i < 3; $i++) { }], X::Obsolete,
    'a for over three parenthesized parts is a C-style loop';
throws-like q[for (;;) { }], X::Obsolete,
    'a for over three empty parenthesized parts is a C-style loop';
throws-like q[for (my $i = 0 ; $i < 3 ; $i++) { }], X::Obsolete,
    'a for over three parenthesized parts with spaces before the semicolons is a C-style loop';
throws-like q[for (my $i = 0; $i < 3;) { }], X::Obsolete,
    'a for whose third parenthesized part is empty is a C-style loop';
throws-like "for (;\n1;\n2)\n\{ }", X::Obsolete, line => 1, pre => / 'for ' $ /,
    'the C-style loop error points at the parenthesized parts';
throws-like q[for (my $i = 0; $i < 3; $i++){ }], X::Obsolete,
    'a for over three parenthesized parts with no space before its block is a C-style loop';
throws-like q[for (1; 2; 3).map(* + 1) { }], X::Obsolete,
    'a for whose source starts with three parenthesized parts is a C-style loop';

is EVAL(q[my @seen; for (1; 2) { @seen.push($_) }; @seen.join(',')]), '1,2',
    'a for over two parenthesized parts is not a C-style loop';
is EVAL(q[my @seen; for (1; 2; 3; 4) { @seen.push($_) }; @seen.join(',')]), '1,2,3,4',
    'a for over four parenthesized parts is not a C-style loop';
is EVAL(q[my @seen; for ((1; 2; 3)) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for over three parts in doubled parentheses is not a C-style loop';
is EVAL(q[my @seen; for [1; 2; 3] { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for over three bracketed parts is not a C-style loop';
lives-ok { EVAL "for (\{ 1 }\n\{ 2 }; 3; 4) \{ }" },
    'a for whose first parenthesized part holds two statements is not a C-style loop';
is EVAL(q[my @seen; for (1 if 1; 2; 3) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for whose first parenthesized part has a condition modifier is not a C-style loop';
is EVAL(q[my @seen; for (1 for 1; 2; 3) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for whose first parenthesized part has a loop modifier is not a C-style loop';
is EVAL(q[my @seen; for (L: 1; 2; 3) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for whose first parenthesized part has a label is not a C-style loop';
is EVAL(q[use isms <Perl5>; my @seen; for (1; 2; 3) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
    'a for over three parenthesized parts iterates them under Perl 5 isms';

if $rakuast {
    is EVAL(q[my @seen; for (if 1 { 1 }; 2; 3) { @seen.push($_) }; @seen.join(',')]), '1,2,3',
        'a for whose first parenthesized part is a statement control is not a C-style loop';
}
else {
    skip 'the legacy frontend parses the statement control as a listop', 1;
}

if $rakuast {
    my $slang = q:to/CODE/;
        role ForAgain {
            rule statement-control:sym<for> {
                <.block-for><.kok> {}
                <.vetPerlForSyntax>
                :my $*GOAL := '{';
                :my $*BORG := {};
                <EXPR>
                <pointy-block>
            }
        }
        BEGIN $*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin(ForAgain), $*LANG.slang_actions('MAIN'));
        CODE
    throws-like $slang ~ q[for (;;) { }], X::Obsolete,
        'a slang for rule that calls vetPerlForSyntax reports a C-style loop';
    is EVAL($slang ~ q[for (my $n = 1)..3 { }; $n]), 1,
        'a slang for rule that calls vetPerlForSyntax parses its source once';
    is EVAL(q:to/CODE/), '1,2,3',
        role AllowCStyle { token vetPerlForSyntax { <?> } }
        BEGIN $*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin(AllowCStyle), $*LANG.slang_actions('MAIN'));
        my @seen; for (1; 2; 3) { @seen.push($_) }; @seen.join(',')
        CODE
        'a slang overriding vetPerlForSyntax iterates three parenthesized parts';
}
else {
    skip 'the legacy grammar has no vetPerlForSyntax', 3;
}
