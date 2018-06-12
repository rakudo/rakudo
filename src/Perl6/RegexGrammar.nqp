# This file is not stand-alone as it is part of another file: it will
# be concatenated with src/Perl6/Grammar.nqp during the build process.
#
# Files should be concatenated in this order (see
# tools/build/Makefile*[JVM|Moarvm]*in):
#
#   Grammar.nqp
#   QGrammar,nqp
#   RegexGrammar.nqp
#   P5RegexGrammar.nqp
#   PodGrammar.nqp

grammar Perl6::RegexGrammar is QRegex::P6Regex::Grammar does STD does MatchPackageNibbler {
    method nibbler() {
        self.nibble-in-cursor(QRegex::P6Regex::Grammar)
    }

    method throw_unrecognized_metachar ($metachar) {
        self.typed_sorry('X::Syntax::Regex::UnrecognizedMetachar', :$metachar);
    }
    method throw_null_pattern() {
        self.typed_sorry('X::Syntax::Regex::NullRegex');
    }
    method throw_unrecognized_regex_modifier($modifier) {
        self.typed_panic('X::Syntax::Regex::UnrecognizedModifier', :$modifier);
    }

    method throw_malformed_range() { self.typed_sorry('X::Syntax::Regex::MalformedRange') }
    method throw_confused() { self.typed_sorry('X::Syntax::Confused') }
    method throw_unspace($char) { self.typed_sorry('X::Syntax::Regex::Unspace', :$char) }
    method throw_regex_not_terminated() { self.typed_sorry('X::Syntax::Regex::Unterminated') }
    method throw_spaces_in_bare_range() { self.typed_sorry('X::Syntax::Regex::SpacesInBareRange') }
    method throw_non_quantifiable() { self.typed_sorry('X::Syntax::Regex::NonQuantifiable') }
    method throw_solitary_quantifier() { self.typed_panic('X::Syntax::Regex::SolitaryQuantifier') }
    method throw_solitary_backtrack_control() { self.typed_sorry('X::Syntax::Regex::SolitaryBacktrackControl') }

    token normspace { <?before \s | '#'> <.LANG('MAIN', 'ws')> }

    token rxstopper { <stopper> }

    token metachar:sym<:my> {
        ':' <?before 'my'|'constant'|'state'|'our'|'temp'|'let'> <statement=.LANG('MAIN', 'statement')>
        <!RESTRICTED>
        <.LANG('MAIN', 'eat_terminator')>
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    token metachar:sym<rakvar> {
        <?before <.sigil> $<twigil>=[<.alpha> | \W<.alpha> | '(']>
        <!before <.sigil> <.rxstopper> >
        <var=.LANG('MAIN', 'variable')>
        [
        || $<binding> = ( \s* '=' \s* <quantified_atom> )
           { self.check_variable($<var>) unless $<twigil> eq '<' }
        || { self.check_variable($<var>) }
           [ <?before '.'? <.[ \[ \{ \< ]>> <.worry: "Apparent subscript will be treated as regex"> ]?
        ]
        <.SIGOK>
    }

    token metachar:sym<qw> {
        <?before '<' \s >  # (note required whitespace)
        '<' <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w']))> '>'
        <.SIGOK>
    }

    token metachar:sym<'> { <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')> <.SIGOK> }

    token metachar:sym<{}> { \\<[xo]>'{' <.obsbrace> }

    token backslash:sym<1> {
        <.[\d] - [0]>\d*
        {}
        :my int $br := nqp::radix(10, $/, 0, 0)[0];
        <.sorry("Unrecognized backslash sequence (did you mean \${$br == 0 ?? $br !! $br - 1}?)")>
    }

    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<?{ }> {
        '?' <?before '{'> <codeblock>
    }

    token assertion:sym<!{ }> {
        '!' <?before '{'> <codeblock>
    }

    token assertion:sym<var> {
        [
        | <?[&]> <!RESTRICTED> <var=.LANG('MAIN', 'term:sym<variable>')>
            [
            | ':' <arglist>
            | '(' <arglist> ')'
            ]?
        | <?sigil> <!RESTRICTED> <var=.LANG('MAIN', 'term:sym<variable>')>
        ]
    }

    token assertion:sym<~~> {
        <sym>
        <!RESTRICTED>
        [ <?[>]> | $<num>=[\d+] | <desigilname=.LANG('MAIN','desigilname')> ]
    }

    token codeblock {
        :my $*ESCAPEBLOCK := 1;
        <!RESTRICTED>
        <block=.LANG('MAIN','block')>
    }

    token arglist {
        :my $*IN_REGEX_ASSERTION := 1;
        <!RESTRICTED>
        <arglist=.LANG('MAIN','arglist')>
    }

    token assertion:sym<name> {
        <longname=.LANG('MAIN','longname')>
            [
            | <?[>]>
            | '=' <assertion>
            | ':' <arglist>
            | '(' <arglist> ')'
            | <.normspace> <nibbler>
            ]?
    }
}

# vim: ft=perl6 et sw=4
