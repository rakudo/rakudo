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

grammar Perl6::QGrammar is HLL::Grammar does STD {

    method throw_unrecog_backslash_seq ($sequence) {
        self.typed_sorry('X::Backslash::UnrecognizedSequence', :$sequence);
    }

    proto token escape {*}
    proto token backslash {*}

    role b1 {
        token escape:sym<\\> { <sym> {} <item=.backslash> }
        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:delim { <text=.starter> | <text=.stopper> }
        token backslash:sym<a> { <sym> }
        token backslash:sym<b> { <sym> }
        token backslash:sym<c> { <sym> <charspec> }
        token backslash:sym<e> { <sym> }
        token backslash:sym<f> { <sym> }
        token backslash:sym<N> { <?before 'N{'<.[A..Z]>> <.obs('\N{CHARNAME}','\c[CHARNAME]')>  }
        token backslash:sym<n> { <sym> }
        token backslash:sym<o> { :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ] }
        token backslash:sym<r> { <sym> }
        token backslash:sym<rn> { 'r\n' }
        token backslash:sym<t> { <sym> }
        token backslash:sym<x> { :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ] }
        token backslash:sym<0> { <sym> }
        token backslash:sym<1> { <[1..9]>\d* {} <.sorry("Unrecognized backslash sequence (did you mean \${$/ - 1}?)")> }
        token backslash:sym<unrec> { {} (\w) { self.throw_unrecog_backslash_seq: $/[0].Str } }
        token backslash:sym<misc> { \W }
    }

    role b0 {
        token escape:sym<\\> { <!> }
    }

    role c1 {
        token escape:sym<{ }> { :my $*ESCAPEBLOCK := 1; <?[{]> <!RESTRICTED> <block=.LANG('MAIN','block')> }
    }

    role c0 {
        token escape:sym<{ }> { <!> }
    }

    role s1 {
        token escape:sym<$> {
            :my $*QSIGIL := '$';
            <?[$]>
            <!RESTRICTED>
            [ <EXPR=.LANG('MAIN', 'EXPR', 'y=')> || { $*W.throw($/, 'X::Backslash::NonVariableDollar') } ]
        }
    }

    role s0 {
        token escape:sym<$> { <!> }
    }

    role a1 {
        token escape:sym<@> {
            :my $*QSIGIL := '@';
            <?[@]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role a0 {
        token escape:sym<@> { <!> }
    }

    role h1 {
        token escape:sym<%> {
            :my $*QSIGIL := '%';
            <?[%]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role h0 {
        token escape:sym<%> { <!> }
    }

    role f1 {
        token escape:sym<&> {
            :my $*QSIGIL := '&';
            <?[&]>
            <!RESTRICTED>
            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
        }
    }

    role f0 {
        token escape:sym<&> { <!> }
    }

    role ww {
        token escape:sym<'> {
            <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')>
        }
        token escape:sym<colonpair> {
            <?[:]> <!RESTRICTED> <colonpair=.LANG('MAIN','colonpair')>
        }
        token escape:sym<#> {
            <?[#]> <.LANG('MAIN', 'comment')>
        }
    }

    role to[$herelang] {
        method herelang() { $herelang }
        method postprocessors () { nqp::list_s('heredoc') } # heredoc strings are the only postproc when present
    }

    role q {
        token starter { \' }
        token stopper { \' }

        token escape:sym<\\> { <sym> <item=.backslash> }

        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:delim { <text=.starter> | <text=.stopper> }

        token backslash:sym<miscq> { {} . }

        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
        token starter { \" }
        token stopper { \" }
        method tweak_q($v) { self.panic("Too late for :q") }
        method tweak_qq($v) { self.panic("Too late for :qq") }
    }

    token nibbler {
        :my @*nibbles;
        <.do_nibbling>
    }

    token do_nibbling {
        :my $from := self.pos;
        :my $to   := $from;
        [
            <!stopper>
            [
            || <starter> <nibbler> <stopper>
                {
                    my $c := $/;
                    $to   := $<starter>[-1].from;
                    if $from != $to {
                        nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
                    }

                    nqp::push(@*nibbles, $<starter>[-1].Str);
                    nqp::push(@*nibbles, $<nibbler>[-1]);
                    nqp::push(@*nibbles, $<stopper>[-1].Str);

                    $from := $to := $c.pos;
                }
            || <escape>
                {
                    my $c := $/;
                    $to   := $<escape>[-1].from;
                    if $from != $to {
                        nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
                    }

                    nqp::push(@*nibbles, $<escape>[-1]);

                    $from := $to := $c.pos;
                }
            || .
            ]
        ]*
        {
            my $c := $/;
            $to   := $c.pos;
            $*LASTQUOTE := [self.pos, $to];
            if $from != $to || !@*nibbles {
                nqp::push(@*nibbles, nqp::substr($c.orig, $from, $to - $from));
            }
        }
    }

    role cc {
        token starter { \' }
        token stopper { \' }

        method ccstate ($s) {
            if $*CCSTATE eq '..' {
                $*CCSTATE := '';
            }
            else {
                $*CCSTATE := $s;
            }
            self;
        }

        # (must not allow anything to match . in nibbler or we'll lose track of state)
        token escape:ws { \s+ [ <?[#]> <.ws> ]? }
        token escape:sym<#> { '#' <.panic: "Please backslash # for literal char or put whitespace in front for comment"> }
        token escape:sym<\\> { <sym> <item=.backslash> <.ccstate('\\' ~ $<item>)> }
        token escape:sym<..> { <sym>
            [
            || <?{ ($*CCSTATE eq '') || ($*CCSTATE eq '..') }> <.sorry("Range missing start character on the left")>
            || <?before \s* <!stopper> <!before '..'> \S >
            || <.sorry("Range missing stop character on the right")>
            ]
            { $*CCSTATE := '..'; }
        }

        token escape:sym<-> {
            '-' <?{ $*CCSTATE ne '' }> \s* <!stopper> \S
            <.obs('- as character range','.. (or \\- if you mean a literal hyphen)')>
        }
        token escape:ch { $<ch> = [\S] { self.ccstate($<ch>) } }

        token backslash:delim { <text=.starter> | <text=.stopper> }
        token backslash:<\\> { <text=.sym> }
        token backslash:sym<a> { :i <sym> }
        token backslash:sym<b> { :i <sym> }
        token backslash:sym<c> { :i <sym> <charspec> }
        token backslash:sym<d> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<e> { :i <sym> }
        token backslash:sym<f> { :i <sym> }
        token backslash:sym<h> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<N> { <?before 'N{'<.[A..Z]>> <.obs('\N{CHARNAME}','\c[CHARNAME]')>  }
        token backslash:sym<n> { :i <sym> }
        token backslash:sym<o> { :i :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ] }
        token backslash:sym<r> { :i <sym> }
        token backslash:sym<s> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<t> { :i <sym> }
        token backslash:sym<v> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<w> { :i <sym> { $*CCSTATE := '' } }
        token backslash:sym<x> { :i :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ] }
        token backslash:sym<0> { <sym> }

        # keep random backslashes like qq does
        token backslash:misc { {}
            [
            | $<text>=(\W)
            | $<x>=(\w) <.sorry("Unrecognized backslash sequence: '\\" ~ $<x> ~ "'")>
            ]
        }
        multi method tweak_q($v) { self.panic("Too late for :q") }
        multi method tweak_qq($v) { self.panic("Too late for :qq") }
        multi method tweak_cc($v) { self.panic("Too late for :cc") }
    }

    method truly($bool, $opt) {
        self.sorry("Cannot negate $opt adverb") unless $bool;
        self;
    }

    method apply_tweak($role) {
        my $target := nqp::can(self, 'herelang') ?? self.herelang !! self;
        $target.HOW.mixin($target, $role);
        self
    }

    method tweak_q($v)          { self.truly($v, ':q'); self.apply_tweak(Perl6::QGrammar::q) }
    method tweak_single($v)     { self.tweak_q($v) }
    method tweak_qq($v)         { self.truly($v, ':qq'); self.apply_tweak(Perl6::QGrammar::qq); }
    method tweak_double($v)     { self.tweak_qq($v) }

    method tweak_b($v)          { self.apply_tweak($v ?? b1 !! b0) }
    method tweak_backslash($v)  { self.tweak_b($v) }
    method tweak_s($v)          { self.apply_tweak($v ?? s1 !! s0) }
    method tweak_scalar($v)     { self.tweak_s($v) }
    method tweak_a($v)          { self.apply_tweak($v ?? a1 !! a0) }
    method tweak_array($v)      { self.tweak_a($v) }
    method tweak_h($v)          { self.apply_tweak($v ?? h1 !! h0) }
    method tweak_hash($v)       { self.tweak_h($v) }
    method tweak_f($v)          { self.apply_tweak($v ?? f1 !! f0) }
    method tweak_function($v)   { self.tweak_f($v) }
    method tweak_c($v)          { self.apply_tweak($v ?? c1 !! c0) }
    method tweak_closure($v)    { self.tweak_c($v) }

    method add-postproc(str $newpp) {
        my $target := nqp::can(self, 'herelang') ?? self.herelang !! self;

        my @pplist := nqp::can($target, "postprocessors")
            ?? $target.postprocessors
            !! nqp::list_s();
        nqp::push_s(@pplist, $newpp);

        # yes, the currying is necessary. Otherwise weird things can happen,
        # e.g.  perl6 -e 'q:w:x//; q:ww:v//' turning the second into q:w:x:v//
        role postproc[@curlist] {
            method postprocessors() {
                @curlist;
            }
        }
        $target.HOW.mixin($target, postproc.HOW.curry(postproc, @pplist));
        self
    }

# path() NYI
#    method tweak_p($v)          { $v ?? self.add-postproc("path") !! self }
#    method tweak_path($v)       { self.tweak_p($v) }

    method tweak_x($v)          { $v ?? self.add-postproc("run") !! self }
    method tweak_exec($v)       { self.tweak_x($v) }
    method tweak_w($v)          { $v ?? self.add-postproc("words") !! self }
    method tweak_words($v)      { self.tweak_w($v) }
    method tweak_ww($v)         { $v ?? self.add-postproc("quotewords").apply_tweak(ww) !! self }
    method tweak_quotewords($v) { self.tweak_ww($v) }

    method tweak_v($v)          { $v ?? self.add-postproc("val") !! self }
    method tweak_val($v)        { self.tweak_v($v) }

    method tweak_cc($v)         { self.truly($v, ':cc'); self.apply_tweak(cc); }

    method tweak_to($v) {
        self.truly($v, ':to');
        # the cursor_init is to ensure it's been initialized the same way
        # 'self' was back in quote_lang
        my $q := self.slang_grammar('Quote');
        $q.HOW.mixin($q, to.HOW.curry(to, self)).'!cursor_init'(self.orig(), :p(self.pos()), :shared(self.'!shared'()))
    }
    method tweak_heredoc($v)    { self.tweak_to($v) }

    method tweak_regex($v) {
        self.truly($v, ':regex');
        return self.slang_grammar('Regex');
    }
}

my role MatchPackageNibbler {
    method nibble-in-cursor($parent) {
        my $*LEAF := self;
        my $*PACKAGE := $*W.find_symbol(['Match']); self.set_package($*PACKAGE);
        my %*ATTR_USAGES;
        my $cur := nqp::findmethod($parent, 'nibbler')(self);
        for %*ATTR_USAGES {
            my $name := $_.key;
            my $node := $_.value[0].node;
            $node.typed_sorry('X::Attribute::Regex', symbol => $name);
        }
        $cur
    }
}

# vim: ft=perl6 et sw=4
