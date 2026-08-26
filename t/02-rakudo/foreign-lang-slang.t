use Test;
use nqp;
use experimental :rakuast;

plan 6;

unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    skip-rest 'FOREIGN-LANG is the RakuAST frontend spelling';
    exit;
}

grammar Digits::Lang {
    token main-syntax { \d+ }
    token exactly($n) { \d ** {$n} }
    token trimmed { '#' <( \d+ )> '#' }
    token based($n, :$hex) {
        [ <?{ $hex }> <[0..9a..f]> ** {$n} | <?{ !$hex }> \d ** {$n} ]
    }
}
class Digits::Actions {
    method main-syntax($/) { make (~$/).Int }
    method exactly($/)     { make (~$/).Int }
}
my sub value-of(Mu $/) { nqp::atkey($/.hash, 'value') }
my role Digits::Slang {
    token term:sym<digitize> {
        <sym> \s* '{' \s* <value=.FOREIGN-LANG('Digits::Lang', 'main-syntax')> \s* '}'
    }
    token term:sym<two-digits> {
        <sym> \s* '{' \s* <value=.FOREIGN-LANG('Digits::Lang', 'exactly', 2)> \s* '}'
    }
    token term:sym<trim-digits> {
        <sym> \s* '{' \s* <value=.FOREIGN-LANG('Digits::Lang', 'trimmed')> \s* '}'
    }
    token term:sym<hex-digits> {
        <sym> \s* '{' \s* <value=.FOREIGN-LANG('Digits::Lang', 'based', 2, :hex)> \s* '}'
    }
}
my role Digits::SlangActions {
    method term:sym<digitize>(Mu $/) {
        self.attach: $/, RakuAST::IntLiteral.new(value-of($/).made);
    }
    method term:sym<two-digits>(Mu $/) {
        self.attach: $/, RakuAST::IntLiteral.new(value-of($/).made);
    }
    method term:sym<trim-digits>(Mu $/) {
        self.attach: $/, RakuAST::StrLiteral.new(value-of($/).Str);
    }
    method term:sym<hex-digits>(Mu $/) {
        self.attach: $/, RakuAST::StrLiteral.new(value-of($/).Str);
    }
}

my $setup = Q:to/SETUP/;
    BEGIN {
        $*LANG.define_slang('MAIN',
          $*LANG.slang_grammar('MAIN').^mixin(Digits::Slang),
          $*LANG.slang_actions('MAIN').^mixin(Digits::SlangActions));
        $*LANG.define_slang('Digits::Lang', Digits::Lang, Digits::Actions);
    }
    SETUP

is EVAL($setup ~ 'digitize { 42 }'), 42,
    'the made value of the foreign parse reaches the calling action';

is EVAL($setup ~ 'digitize { 42 } + 58'), 100,
    'parsing continues after the region the foreign grammar consumed';

is EVAL($setup ~ 'two-digits { 42 }'), 42,
    'extra FOREIGN-LANG arguments are passed to the foreign regex';

is EVAL($setup ~ 'trim-digits { #42# }'), "42",
    'capture markers in the foreign token trim the matched text';

is EVAL($setup ~ 'hex-digits { ab }'), "ab",
    'named FOREIGN-LANG arguments are passed to the foreign regex';

throws-like { EVAL($setup ~ 'digitize { nope }') }, X::Undeclared::Symbols,
    'a failed foreign parse falls back to ordinary compile errors';

# vim: expandtab shiftwidth=4
