use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use RoleBodyLexicals;

plan 15;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

# Loading the module precompiles it, so each role's methods reach the
# lexicals of the module through the role body's lexical fixup.

is RoleBodyLexicals::hash-composer(), 7,
    'a role declared in a hash composer in a precompiled module sees the lexicals around it';
todo 'gives a wrong value on the legacy frontend', 14 unless $rakuast;
is RoleBodyLexicals::for-modifier(), 7,
    'a role declared as a for modifier statement in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::for-modifier-over-nothing(), 7,
    'a role declared as a for modifier statement over nothing in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::under-try(), 7,
    'a role declared under try in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::under-gather(), 7,
    'a role declared under gather in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::in-thunk-not-run(), 7,
    'a role declared in a thunk that does not run in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::constant-andthen(), 7,
    'a role declared right of andthen in a constant of a precompiled module sees the lexicals around it';
is RoleBodyLexicals::constant-andthen-parens(), 7,
    'a parenthesized role declared right of andthen in a constant of a precompiled module sees the lexicals around it';
is RoleBodyLexicals::constant-array(), 7,
    'a role declared in an array literal in a constant of a precompiled module sees the lexicals around it';
is RoleBodyLexicals::constant-for-modifier(), 7,
    'a role declared as a for modifier statement in a constant of a precompiled module sees the lexicals around it';
is RoleBodyLexicals::body-lexical(), 8,
    'a role declared as a for modifier statement in a precompiled module has a body that sees the lexicals around it';
is RoleBodyLexicals::under-once(), 7,
    'a role declared under once in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::class-trait(), 7,
    'a role declared in a trait argument of a class in a precompiled module sees the lexicals around it';
# These hold without the thunk too, and guard what it keeps.
is RoleBodyLexicals::right-of-andthen-not-run(), 7,
    'a role declared right of andthen that does not run in a precompiled module sees the lexicals around it';
is RoleBodyLexicals::right-of-orelse-not-run(), 7,
    'a role declared right of orelse that does not run in a precompiled module sees the lexicals around it';
