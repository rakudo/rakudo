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

        # Variables used during the parse.
        my $*LEFTSIGIL;                           # sigil of LHS for item vs list assignment
        my $*IN_META := '';                       # parsing a metaoperator like [..]
        my $*IN_REDUCE := 0;                      # attempting to parse an [op] construct

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

        { $*R.enter-scope($*CU) }
        <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>
        [ $ || <.typed_panic: 'X::Syntax::Confused'> ]
        { $*R.leave-scope() }
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

    # Precedence levels and their defaults
    my %methodcall      := nqp::hash('prec', 'y=', 'assoc', 'unary', 'dba', 'methodcall', 'fiddly', 1);
    my %autoincrement   := nqp::hash('prec', 'x=', 'assoc', 'unary', 'dba', 'autoincrement');
    my %exponentiation  := nqp::hash('prec', 'w=', 'assoc', 'right', 'dba', 'exponentiation');
    my %symbolic_unary  := nqp::hash('prec', 'v=', 'assoc', 'unary', 'dba', 'symbolic unary');
    my %dottyinfix      := nqp::hash('prec', 'v=', 'assoc', 'left', 'dba', 'dotty infix', 'nextterm', 'dottyopish', 'sub', 'z=', 'fiddly', 1);
    my %multiplicative  := nqp::hash('prec', 'u=', 'assoc', 'left', 'dba', 'multiplicative');
    my %additive        := nqp::hash('prec', 't=', 'assoc', 'left', 'dba', 'additive');
    my %replication     := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication');
    my %replication_xx  := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication', 'thunky', 't.');
    my %concatenation   := nqp::hash('prec', 'r=', 'assoc', 'left', 'dba', 'concatenation');
    my %junctive_and    := nqp::hash('prec', 'q=', 'assoc', 'list', 'dba', 'junctive and');
    my %junctive_or     := nqp::hash('prec', 'p=', 'assoc', 'list', 'dba', 'junctive or');
    my %named_unary     := nqp::hash('prec', 'o=', 'assoc', 'unary', 'dba', 'named unary');
    my %structural      := nqp::hash('prec', 'n=', 'assoc', 'non', 'dba', 'structural infix', 'diffy', 1);
    my %chaining        := nqp::hash('prec', 'm=', 'assoc', 'left', 'dba', 'chaining', 'iffy', 1, 'diffy', 1);
    my %tight_and       := nqp::hash('prec', 'l=', 'assoc', 'left', 'dba', 'tight and', 'thunky', '.t');
    my %tight_or        := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or', 'thunky', '.t');
    my %tight_or_minmax := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or');
    my %conditional     := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', '.tt');
    my %conditional_ff  := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', 'tt');
    my %item_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'item assignment');
    my %list_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'list assignment', 'sub', 'e=', 'fiddly', 1);
    my %loose_unary     := nqp::hash('prec', 'h=', 'assoc', 'unary', 'dba', 'loose unary');
    my %comma           := nqp::hash('prec', 'g=', 'assoc', 'list', 'dba', 'comma', 'nextterm', 'nulltermish', 'fiddly', 1);
    my %list_infix      := nqp::hash('prec', 'f=', 'assoc', 'list', 'dba', 'list infix');
    my %list_prefix     := nqp::hash('prec', 'e=', 'assoc', 'right', 'dba', 'list prefix');
    my %loose_and       := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.t');
    my %loose_andthen   := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.b');
    my %loose_or        := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.t');
    my %loose_orelse    := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.b');
    my %sequencer       := nqp::hash('prec', 'b=', 'assoc', 'list', 'dba', 'sequencer');

    method EXPR(str $preclim = '') {
        my $*LEFTSIGIL := '';
        nqp::findmethod(HLL::Grammar, 'EXPR')(self, $preclim, :noinfix($preclim eq 'y='));
    }

    token infixish($in_meta = nqp::getlexdyn('$*IN_META')) {
        :my $*IN_META := $in_meta;
        :my $*OPER;
        <!stdstopper>
        <!infixstopper>
        :dba('infix')
        [
#        | <!{ $*IN_REDUCE }> <colonpair> <fake_infix> { $*OPER := $<fake_infix> }
        |   [
#            | :dba('bracketed infix') '[' ~ ']' <infixish('[]')> { $*OPER := $<infixish><OPER> }
#                # XXX Gets false positives.
#            | :dba('infixed function') <?before '[&' <twigil>? [<alpha>|'('] > '[' ~ ']' <variable>
#                {
#                    $<variable><O> := self.O(:prec<t=>, :assoc<left>, :dba<additive>).MATCH unless $<variable><O>;
#                    $*OPER := $<variable>;
#                    self.check_variable($<variable>)
#                }
#            | <infix_circumfix_meta_operator> { $*OPER := $<infix_circumfix_meta_operator> }
#            | <infix_prefix_meta_operator> { $*OPER := $<infix_prefix_meta_operator> }
            | <infix> { $*OPER := $<infix> }
            | <?{ $*IN_META ~~ /^[ '[]' | 'hyper' | 'HYPER' | 'R' | 'S' ]$/ && !$*IN_REDUCE }> <.missing("infix inside " ~ $*IN_META)>
            ]
#            [ <?before '='> <infix_postfix_meta_operator> { $*OPER := $<infix_postfix_meta_operator> }
#            ]?
        ]
        <OPER=.AS_MATCH($*OPER)>
        { nqp::bindattr_i($<OPER>, NQPMatch, '$!pos', $*OPER.pos); }
    }

    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before '!!'> <?{ $*GOAL eq '!!' }>
        | <?before '{' | <.lambda> > <?MARKED('ws')> <?{ $*GOAL eq '{' || $*GOAL eq 'endargs' }>
        ]
    }

    token prefixish {
        :dba('prefix')
        [
        | <OPER=prefix>
#        | <OPER=prefix_circumfix_meta_operator>
        ]
#        <prefix_postfix_meta_operator>**0..1
        <.ws>
    }

    token postfixish {
        <!stdstopper>

        # last whitespace didn't end here
        <?{
            my $c := $/;
            my $marked := $c.MARKED('ws');
            !$marked || $marked.from == $c.pos;
        }>

        [ <!{ $*QSIGIL }> [ <.unsp> | '\\' ] ]?

        :dba('postfix')
#        [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>?]**0..1
        [
        | <OPER=postfix>
        | '.' <?before \W> <OPER=postfix>  ## dotted form of postfix operator (non-wordy only)
#        | <OPER=postcircumfix>
#        | '.' <?[ [ { < ]> <OPER=postcircumfix>
#        | <OPER=dotty>
#        | <OPER=privop>
#        | <?{ $<postfix_prefix_meta_operator> && !$*QSIGIL }>
#            [
#            || <?space> <.missing: "postfix">
#            || <?alpha> <.missing: "dot on method call">
#            || <.malformed: "postfix">
#            ]
        ]
        { $*LEFTSIGIL := '@'; }
    }

    token postop {
        | <postfix>         $<O> = {$<postfix><O>} $<sym> = {$<postfix><sym>}
#        | <postcircumfix>   $<O> = {$<postcircumfix><O>} $<sym> = {$<postcircumfix><sym>}
    }

    method AS_MATCH($v) {
        self.'!clone_match_at'($v,self.pos());
    }

    token prefix:sym<++>  { <sym>  <O(|%autoincrement)> }
    token prefix:sym<-->  { <sym>  <O(|%autoincrement)> }
    token prefix:sym<++⚛> { <sym>  <O(|%autoincrement)> }
    token prefix:sym<--⚛> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<++> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<--> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<⚛++> { <sym>  <O(|%autoincrement)> }
    token postfix:sym<⚛--> { <sym>  <O(|%autoincrement)> }

    token infix:sym<**>   { <sym>  <O(|%exponentiation)> }

    token prefix:sym<+>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<~~>  { <sym> <.dupprefix('~~')> <O(|%symbolic_unary)> }
    token prefix:sym<~>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<->   { <sym> <O(|%symbolic_unary)> }
    token prefix:sym<−>   { <sym> <O(|%symbolic_unary)> }
    token prefix:sym<??>  { <sym> <.dupprefix('??')> <O(|%symbolic_unary)> }
    token prefix:sym<?>   { <sym> <!before '??'> <O(|%symbolic_unary)> }
    token prefix:sym<!>   { <sym> <!before '!!'> <O(|%symbolic_unary)> }
    token prefix:sym<|>   { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<+^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<~^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<?^>  { <sym>  <O(|%symbolic_unary)> }
    token prefix:sym<^^>  { <sym> <.dupprefix('^^')> <O(|%symbolic_unary)> }
    token prefix:sym<^>   {
        <sym>  <O(|%symbolic_unary)>
        <?before \d+ <?before \. <.?alpha> > <.worry: "Precedence of ^ is looser than method call; please parenthesize"> >?
    }
    token prefix:sym<⚛>   { <sym>  <O(|%symbolic_unary)> }

    token infix:sym<*>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<×>    { <sym>  <O(|%multiplicative)> }
    token infix:sym</>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<÷>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<div>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<gcd>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<lcm>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<%>    { <sym>  <O(|%multiplicative)> }
    token infix:sym<mod>  { <sym> >> <O(|%multiplicative)> }
    token infix:sym<%%>   { <sym>  <O(|%multiplicative, :iffy(1))> }
    token infix:sym<+&>   { <sym>  <O(|%multiplicative)> }
    token infix:sym<~&>   { <sym>  <O(|%multiplicative)> }
    token infix:sym<?&>   { <sym>  <O(|%multiplicative, :iffy(1))> }
    token infix:sym«+<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
    token infix:sym«+>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }
    token infix:sym«~<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
    token infix:sym«~>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }

    token infix:sym«<<» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('<< to do left shift', '+< or ~<')> <O(|%multiplicative)> }

    token infix:sym«>>» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('>> to do right shift', '+> or ~>')> <O(|%multiplicative)> }

    token infix:sym<+>    { <sym>  <O(|%additive)> }
    token infix:sym<->    {
       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
        <sym> [<?before '>>'> || <![>]>]
        <O(|%additive)>
    }
    token infix:sym<−>    { <sym>  <O(|%additive)> }
    token infix:sym<+|>   { <sym>  <O(|%additive)> }
    token infix:sym<+^>   { <sym>  <O(|%additive)> }
    token infix:sym<~|>   { <sym>  <O(|%additive)> }
    token infix:sym<~^>   { <sym>  <O(|%additive)> }
    token infix:sym<?|>   { <sym>  <O(|%additive, :iffy(1))> }
    token infix:sym<?^>   { <sym>  <O(|%additive, :iffy(1))> }

    token infix:sym<x>    { <sym> >> <O(|%replication)> }
    token infix:sym<xx>    { <sym> >> <O(|%replication_xx)> }

    token infix:sym<~>    { <sym>  <O(|%concatenation)> }
    token infix:sym<.>    { <sym> <ws>
        <!{ $*IN_REDUCE }>
        [<!alpha>
            {
                my $pre := nqp::substr(self.orig, self.from - 1, 1);
                $<ws> ne ''
                ?? $¢.obs('. to concatenate strings', '~')
                !! $pre ~~ /^\s$/
                    ?? $¢.malformed('postfix call (only basic method calls that exclusively use a dot can be detached)')
                    !! $¢.malformed('postfix call')
            }
        ]?
        <O(|%dottyinfix)>
    }
    token infix:sym<∘>   { <sym>  <O(|%concatenation)> }
    token infix:sym<o>   { <sym>  <O(|%concatenation)> }

    token infix:sym<&>   { <sym> <O(|%junctive_and, :iffy(1))> }
    token infix:sym<(&)> { <sym> <O(|%junctive_and)> }
    token infix:sym«∩»   { <sym> <O(|%junctive_and)> }
    token infix:sym<(.)> { <sym> <O(|%junctive_and)> }
    token infix:sym«⊍»   { <sym> <O(|%junctive_and)> }

    token infix:sym<|>    { <sym> <O(|%junctive_or, :iffy(1))> }
    token infix:sym<^>    { <sym> <O(|%junctive_or, :iffy(1))> }
    token infix:sym<(|)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«∪»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(^)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«⊖»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(+)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«⊎»    { <sym> <O(|%junctive_or)> }
    token infix:sym<(-)>  { <sym> <O(|%junctive_or)> }
    token infix:sym«∖»    { <sym> <O(|%junctive_or)> }

    token infix:sym«=>» { <sym> <O(|%item_assignment)> }

    token prefix:sym<so> { <sym><.end_prefix> <O(|%loose_unary)> }
    token prefix:sym<not>  { <sym><.end_prefix> <O(|%loose_unary)> }

    token infix:sym<,>    {
        <.unsp>? <sym> <O(|%comma, :fiddly(0))>
    }

    ##
    ## Terms
    ##

    token termish {
        :my $*SCOPE := "";
        :my $*MULTINESS := "";
        :dba('term')
        # TODO try to use $/ for lookback to check for erroneous
        #      use of pod6 trailing declarator block, e.g.:
        #
        #        #=!
        #
        #      instead of
        #
        #        #=(
        #
        [
        ||  [
            | <prefixish>+
              [ || <term>
                || {} <.panic("Prefix " ~ $<prefixish>[-1].Str
                                        ~ " requires an argument, but no valid term found"
                                        ~ ".\nDid you mean "
                                        ~ $<prefixish>[-1].Str
                                        ~ " to be an opening bracket for a declarator block?"
                      )>
              ]
            | <term>
            ]
        || <!{ $*QSIGIL }> <?before <infixish> {
            $/.typed_panic('X::Syntax::InfixInTermPosition', infix => ~$<infixish>); } >
        || <!>
        ]
        :dba('postfix')
        [
        || <?{ $*QSIGIL }>
            [
            || <?{ $*QSIGIL eq '$' }> [ <postfixish>+! <?{ bracket_ending($<postfixish>) }> ]**0..1
            ||                          <postfixish>+! <?{ bracket_ending($<postfixish>) }>
            ]
        || <!{ $*QSIGIL }> <postfixish>*
        ]
    }

    sub bracket_ending($matches) {
        my $check     := $matches[+$matches - 1];
        my str $str   := $check.Str;
        my $last  := nqp::substr($str, nqp::chars($check) - 1, 1);
        $last eq ')' || $last eq '}' || $last eq ']' || $last eq '>' || $last eq '»'
    }

    token term:sym<fatarrow> {
        <key=.identifier> \h* '=>' <.ws> <val=.EXPR('i<=')>
    }

    token term:sym<variable>           { <variable> }
    token term:sym<scope_declarator>   { <scope_declarator> }
    token term:sym<value>              { <value> }

    token term:sym<identifier> {
        <identifier>
        # <!{ $*W.is_type([~$<identifier>]) }>
        [ <?before <.unsp>? '('> | \\ <?before '('> ]
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

    token variable {
        :my $*IN_META := '';
        [
        | <sigil> <desigilname>
        | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]
        ]
        { $*LEFTSIGIL := nqp::substr(self.orig(), self.from, 1) unless $*LEFTSIGIL }
    }

    ##
    ## Declarations
    ##

    proto token scope_declarator { <...> }
    token scope_declarator:sym<my> { <sym> <scoped('my')> }

    token scoped($*SCOPE) {
        <.end_keyword>
        :dba('scoped declarator')
        [
        || <.ws>
           [
           | <DECL=declarator>
           ]
        || <.malformed($*SCOPE)>
        ]
    }

    token declarator {
        :my $*LEFTSIGIL := '';
        [
        | <variable_declarator>
        ]
    }

    token variable_declarator {
        :my $*IN_DECL := 'variable';
        [
        | <sigil> <desigilname>
        | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]
        # TODO error cases for when yoiu declare something you're not allowed to
        ]
        {
            $*IN_DECL := '';
            $*LEFTSIGIL := nqp::substr(self.orig(), self.from, 1) unless $*LEFTSIGIL;
        }
        [<.ws> <initializer>]?
    }

    token desigilname {
        [
#        | <?before <.sigil> <.sigil> > <variable>
#        | <?sigil>
#            [ <?{ $*IN_DECL }> <.typed_panic: 'X::Syntax::Variable::IndirectDeclaration'> ]?
#            <variable> {
#                $*VAR := $<variable>;
#            }
        | <longname>
        ]
    }

    proto token initializer { <...> }
    token initializer:sym<=> {
        <sym>
        [
            <.ws>
            [
            || <?{ $*LEFTSIGIL eq '$' }> <EXPR('i<=')>
            || <EXPR('e=')>
            ]
            || <.malformed: 'initializer'>
        ]
    }
    token initializer:sym<:=> {
        <sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
    }
    token initializer:sym<::=> {
        <sym> [ <.ws> <EXPR('e=')> <.NYI('"::="')> || <.malformed: 'binding'> ]
    }
#    token initializer:sym<.=> {
#        <sym> [ <.ws> <dottyopish> || <.malformed: 'mutator method call'> ]
#    }

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
#        <!!before ['.' <?before \s | ',' | '=' | ':' <!before  <coloncircumfix <OPER=prefix> > > | <.terminator> | $ > <.typed_sorry: 'X::Syntax::Number::IllegalDecimal'>]? >
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
        | <?stdstopper>
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

    token name {
        [
        | <identifier> <morename>*
        | <morename>+
        ]
    }

    token morename {
        :my $*QSIGIL := '';
        '::'
        [
        ||  <?before '(' | <.alpha> >
            [
            | <identifier>
            | :dba('indirect name') '(' ~ ')' [ <.ws> <EXPR> ]
            ]
        || <?before '::'> <.typed_panic: "X::Syntax::Name::Null">
        || $<bad>=[<.sigil><.identifier>] { my str $b := $<bad>; self.malformed("lookup of ::$b; please use ::('$b'), ::\{'$b'\}, or ::<$b>") }
        ]?
    }

    token longname {
        <name> {} [ <?before ':' <.+alpha+[\< \[ \« ]>> <!RESTRICTED> <colonpair> ]*
    }

    token sigil { <[$@%&]> }

    proto token twigil { <...> }
    token twigil:sym<.> { <sym> <?before \w> }
    token twigil:sym<!> { <sym> <?before \w> }
    token twigil:sym<^> { <sym> <?before \w> }
    token twigil:sym<:> { <sym> <?before \w> }
    token twigil:sym<*> { <sym> <?before \w> }
    token twigil:sym<?> { <sym> <?before \w> }
    token twigil:sym<=> { <sym> <?before \w> }
    token twigil:sym<~> { <sym> <?before \w> }

    token lambda { '->' | '<->' }

    token end_keyword {
        » <!before <.[ \( \\ ' \- ]> || \h* '=>'>
    }

    token end_prefix {
        <.end_keyword> \s*
    }

    token spacey { <?[\s#]> }

    token kok {
        <.end_keyword>
        [
        || <?before <.[ \s \# ]> > <.ws>
        || <?{
                my $n := nqp::substr(self.orig, self.from, self.pos - self.from);
                $*W.is_name([$n]) || $*W.is_name(['&' ~ $n])
                    ?? False
                    !! self.panic("Whitespace required after keyword '$n'")
           }>
        ]
    }

    proto token terminator { <...> }
    token terminator:sym<;> { <?[;]> }
    token terminator:sym<)> { <?[)]> }
    token terminator:sym<]> { <?[\]]> }
    token terminator:sym<}> { <?[}]> }
    token terminator:sym<ang> { <?[>]> <?{ $*IN_REGEX_ASSERTION }> }
    token terminator:sym<if>     { 'if'     <.kok> }
    token terminator:sym<unless> { 'unless' <.kok> }
    token terminator:sym<while>  { 'while'  <.kok> }
    token terminator:sym<until>  { 'until'  <.kok> }
    token terminator:sym<for>    { 'for'    <.kok> }
    token terminator:sym<given>  { 'given'  <.kok> }
    token terminator:sym<when>   { 'when'   <.kok> }
    token terminator:sym<with>   { 'with'   <.kok> }
    token terminator:sym<without> { 'without' <.kok> }
    token terminator:sym<arrow>  { '-->' }

    token stdstopper {
        [
        || <?MARKED('endstmt')> <?>
        || [
           | <?terminator>
           | $
           ]
       ]
    }

    token unsp {
        \\ <?before \s | '#'>
        :dba('unspace')
        [
        | <.vws>
        | <.unv>
        | <.unsp>
        ]*
    }

    token vws {
        :dba('vertical whitespace')
        [
            [
            | \v
            | '<<<<<<<' {} <?before [.*? \v '=======']: .*? \v '>>>>>>>' > <.sorry: 'Found a version control conflict marker'> \V* \v
            | '=======' {} .*? \v '>>>>>>>' \V* \v   # ignore second half
            ]
        ]+
    }

    token unv {
        :dba('horizontal whitespace')
        [
        | \h+
#        | \h* <.comment>
#        | <?before \h* '=' [ \w | '\\'] > ^^ <.pod_content_toplevel>
        ]
    }

}
