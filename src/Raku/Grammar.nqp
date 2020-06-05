use NQPP6QRegex;
use NQPP5QRegex;
use Raku::Actions;

grammar Raku::Grammar is HLL::Grammar {

    ##
    ## Compilation unit, language version and other entry point bits
    ##

    method TOP() {
        # Set up the language braid.
        my $*LANG := self;
        my $*MAIN := 'MAIN';
        self.define_slang('MAIN',    self.WHAT,            self.actions);
        #self.define_slang('Quote',   Raku::QGrammar,       Raku::QActions);
        #self.define_slang('Regex',   Raku::RegexGrammar,   Raku::RegexActions);
        #self.define_slang('P5Regex', Raku::P5RegexGrammar, Raku::P5RegexActions);
        #self.define_slang('Pod',     Raku::PodGrammar,     Raku::PodActions);

        # Parse a compilation unit.
        self.comp_unit
    }

    token comp_unit {
        <.bom>?

        # Set up compilation unit and symbol resolver according to the language
        # version that is declared, if any.
        :my $*CU;
        :my $*R;
        :my $*LITERALS;
        <.lang_setup>

        <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>
        [ $ || <.typed_panic: 'X::Syntax::Confused'> ]
    }

    token bom { \xFEFF }

    rule lang_setup {
        <?>
    }

    # This is like HLL::Grammar.LANG but it allows to call a token of a Raku level grammar.
    method FOREIGN_LANG($langname, $regex, *@args) {
        my $grammar := self.slang_grammar($langname);
        if nqp::istype($grammar, NQPMatch) {
            self.LANG($langname, $regex, @args);
        }
        else {
            nqp::die('FOREIGN_LANG non-NQP branch NYI')
        }
    }

    ##
    ## Statements
    ##

    rule statementlist($*statement_level = 0) {
        :dba('statement list')
        <.ws>
        # Define this scope to be a new language.
        :my $*LANG;
        <!!{ $*LANG := $/.clone_braid_from(self); 1 }>
        [
        | $
        | <?before <.[\)\]\}]>>
        | [ <statement> <.eat_terminator> ]*
        ]
        <.set_braid_from(self)>   # any language tweaks must not escape
        <!!{ nqp::rebless($/, self.WHAT); 1 }>
    }

    token statement($*LABEL = '') {
        :my $*QSIGIL := '';
        :my $*SCOPE := '';

        :my $actions := self.slang_actions('MAIN');
        <!!{ $/.set_actions($actions); 1 }>
        <!before <.[\])}]> | $ >
        #<!stopper>
        <!!{ nqp::rebless($/, self.slang_grammar('MAIN')); 1 }>

        [
        #| <label> <statement($*LABEL)> { $*LABEL := '' if $*LABEL }
        #| <statement_control>
        | <EXPR>
        | <?[;]>
        #| <?stopper>
        | {} <.panic: "Bogus statement">
        ]
    }

    token eat_terminator {
        || ';'
        || <?MARKED('endstmt')> <.ws>
        || <?before ')' | ']' | '}' >
        || $
        || <?stopper>
        || <?before [if|while|for|loop|repeat|given|when] » > { $/.'!clear_highwater'(); self.typed_panic( 'X::Syntax::Confused', reason => "Missing semicolon" ) }
        || { $/.typed_panic( 'X::Syntax::Confused', reason => "Confused" ) }
    }

    ##
    ## Expression parsing and operators
    ##

    ##
    ## Terms
    ##

    token term:sym<value>              { <value> }

    token term:sym<identifier> {
        <identifier>
        # <!{ $*W.is_type([~$<identifier>]) }> [ <?before <.unsp>? '('> | \\ <?before '('> ]
        <args(1)>
#        {
#            if !$<args><invocant> {
#                self.add_mystery($<identifier>, $<args>.from, nqp::substr(~$<args>, 0, 1));
#                if $*BORG<block> {
#                    unless $*BORG<name> {
#                        $*BORG<name> := ~$<identifier>;
#                    }
#                }
#            }
#        }
    }

    ##
    ## Values
    ##

    proto token value { <...> }
#    token value:sym<quote>  { <quote> }
    token value:sym<number> { <number> }
#    token value:sym<version> { <version> }

    proto token number { <...> }
    token number:sym<numish>   { <numish> }

    token numish {
        [
#        | 'NaN' >>
        | <integer>
#        | <dec_number>
#        | <rad_number>
#        | <rat_number>
#        | <complex_number>
#        | 'Inf' >>
#        | $<uinf>='∞'
#        | <unum=:No+:Nl>
        ]
    }

    token integer {
        [
        | 0 [ b '_'? <VALUE=binint>
            | o '_'? <VALUE=octint>
            | x '_'? <VALUE=hexint>
            | d '_'? <VALUE=decint>
            | <VALUE=decint>
                <!!{ $/.typed_worry('X::Worry::P5::LeadingZero', value => ~$<VALUE>) }>
            ]
        | <VALUE=decint>
        ]
        <!!before ['.' <?before \s | ',' | '=' | ':' <!before  <coloncircumfix <OPER=prefix> > > | <.terminator> | $ > <.typed_sorry: 'X::Syntax::Number::IllegalDecimal'>]? >
        [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
    }

    ##
    ## Argument lists and captures
    ##

    token args($*INVOCANT_OK = 0) {
        :my $*INVOCANT;
        :my $*GOAL := '';
        :dba('argument list')
        [
        | '(' ~ ')' <semiarglist>
        | <.unsp> '(' ~ ')' <semiarglist>
        | [ \s <arglist> ]
        | <?>
        ]
    }

    token semiarglist {
        <arglist>+ % ';'
        <.ws>
    }

    token arglist {
        :my $*GOAL := 'endargs';
        :my $*QSIGIL := '';
        <.ws>
        :dba('argument list')
        [
        #| <?stdstopper>
        | <EXPR('e=')>
        | <?>
        ]
    }

    ##
    ## Lexer stuff
    ##

    token apostrophe {
        <[ ' \- ]>
    }

    token identifier {
        <.ident> [ <.apostrophe> <.ident> ]*
    }
}
