grammar Perl6::Grammar is HLL::Grammar;


method TOP() {
    my %*LANG;
    %*LANG<Regex>         := Perl6::Regex;
    %*LANG<Regex-actions> := Perl6::RegexActions;
    %*LANG<MAIN>          := Perl6::Grammar;
    %*LANG<MAIN-actions>  := Perl6::Actions;
    my %*HOW;
    %*HOW<module>  := 'none';
    %*HOW<class>   := 'ClassHOW';
    %*HOW<grammar> := 'GrammarHOW';
    %*HOW<role>    := 'RoleHOW';
    my %*PKGCOMPILER;
    %*PKGCOMPILER<role>   := Perl6::Compiler::Role;
    %*PKGCOMPILER<module> := Perl6::Compiler::Module;
    my $*SCOPE := '';
    my $*MULTINESS := '';
    self.comp_unit;
}

sub parse_name($name) {
    # XXX Some enterprising soul could re-write this in NQP. ;-)
    Q:PIR {
        .local string name
        $P0 = find_lex '$name'
        name = $P0
        ##  remove any type parameterization for now
        .local string type_param
        type_param = ''
        $I0 = index name, '['
        if $I0 == -1 goto type_param_done
        type_param = substr name, $I0
        name = substr name, 0, $I0
      type_param_done:
        ##  divide name based on ::
        .local pmc list
        list = split '::', name
        ##  move any leading sigil to the last item
        .local string sigil
        $S0 = list[0]
        sigil = substr $S0, 0, 1
        $I0 = index '$@%&', $S1
        if $I0 < 0 goto sigil_done
        substr $S0, 0, 1, ''
        list[0] = $S0
        $S0 = list[-1]
        $S0 = concat sigil, $S0
        list[-1] = $S0
      sigil_done:
        ##  remove any empty items from the list
        $P0 = iter list
        list = new 'ResizablePMCArray'
      iter_loop:
        unless $P0 goto iter_done
        $S0 = shift $P0
        unless $S0 goto iter_loop
        push list, $S0
        goto iter_loop
      iter_done:
        if type_param == '' goto no_add_type_param
        $S0 = pop list
        concat $S0, type_param
        push list, $S0
      no_add_type_param:
        .return (list)
    }
}

method add_my_name($name) {
    my @BLOCK := Q:PIR{ %r = get_hll_global ['Perl6';'Actions'], '@BLOCK' };
    @BLOCK[0].symbol($name, :does_abstraction(1));
    return 1;
}

method is_name($name) {
    # Check in the blocks.
    my @BLOCK := Q:PIR{ %r = get_hll_global ['Perl6';'Actions'], '@BLOCK' };
    for @BLOCK {
        my $sym := $_.symbol($name);
        if $sym && $sym<does_abstraction> {
            return 1;
        }
    }

    # Otherwise, check in the namespace.
    my @parts := parse_name($name);
    my $final := pir::pop(@parts);
    my $test  := pir::get_hll_global__PPS(@parts, $final);
    if !pir::isnull__IP($test) && (pir::does__IPS($test, 'Abstraction') || pir::isa__IPS($test, 'P6protoobject')) {
        return 1;
    }

    # Otherwise, it's a fail.
    return 0;
}

## Lexer stuff

token identifier { <ident> }

token name {
    [
    | <identifier> <morename>*
    | <morename>+
    ]
}

token morename {
    '::' <identifier>
}

token longname {
    <name> <colonpair>*
}

token deflongname { 
    <name> <colonpair>*
}

token ENDSTMT {
    [ \h* $$ <.ws> <?MARKER('endstmt')> ]?
}

## Top-level rules

token comp_unit {
    :my %*METAOPGEN;                           # hash of generated metaops
    :my $*IN_DECL;                             # what declaration we're in
    :my $*IMPLICIT;                            # whether we allow an implicit param
    <.newpad>
    <.finishpad>
    <statementlist> 
    [ $ || <.panic: 'Confused'> ] 
}

rule statementlist {
    | $
    | [<statement><.eat_terminator> ]*
}

token statement {
    <!before <[\])}]> | $ >
    [
    | <statement_control>
    | <EXPR>
    ]
}

token eat_terminator {
    | ';'
    | <?MARKED('endstmt')>
    | <?terminator>
    | $
}

token xblock($*IMPLICIT = 0) {
    <EXPR> <.ws> <pblock($*IMPLICIT)>
}

token pblock($*IMPLICIT = 0) {
    [ <?[{]> || <.panic: 'Missing block'> ]
    <.newpad>
    <blockoid>
}

token block($*IMPLICIT = 0) {
    [ <?[{]> || <.panic: 'Missing block'> ]
    <.newpad>
    <blockoid>
}

token blockoid {
    <.finishpad>
    '{' ~ '}' <statementlist>
    <?ENDSTMT>
}

token newpad { <?> }
token finishpad { <?> }

proto token terminator { <...> }

token terminator:sym<;> { <?[;]> }
token terminator:sym<}> { <?[}]> }

## Statement control

proto token statement_control { <...> }

token statement_control:sym<if> {
    <sym> :s
    <xblock>
    [ 'elsif'\s <xblock> ]*
    [ 'else'\s <else=pblock> ]?
}

token statement_control:sym<unless> {
    <sym> :s
    <xblock>
    [ <!before 'else'> || <.panic: 'unless does not take "else", please rewrite using "if"'> ]
}

token statement_control:sym<while> {
    $<sym>=[while|until] :s
    <xblock>
}

token statement_control:sym<repeat> {
    <sym> :s
    [ 
    | $<wu>=[while|until]\s <xblock>
    | <pblock> $<wu>=[while|until]\s <EXPR> 
    ]
}

token statement_control:sym<for> {
    <sym> :s
    <xblock(1)>
}

token statement_control:sym<return> {
    <sym> :s 
    [ <EXPR> || <.panic: 'return requires an expression argument'> ]
}

token statement_control:sym<make> {
    <sym> :s 
    [ <EXPR> || <.panic: 'make requires an expression argument'> ]
}

token statement_control:sym<use> {
    <sym> <.ws> 'v6'
}

proto token statement_prefix { <...> }
token statement_prefix:sym<INIT> { <sym> <blorst> }

token blorst {
    \s <.ws> [ <block> | <statement> ]
}

## Terms

token term:sym<colonpair>          { <colonpair> }
token term:sym<variable>           { <variable> }
token term:sym<package_declarator> { <package_declarator> }
token term:sym<scope_declarator>   { <scope_declarator> }
token term:sym<routine_declarator> { <routine_declarator> }
token term:sym<multi_declarator>   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
token term:sym<regex_declarator>   { <regex_declarator> }
token term:sym<statement_prefix>   { <statement_prefix> }

token colonpair {
    ':' 
    [ 
    | $<not>='!' <identifier>
    | <identifier> [ <circumfix> | <?> ]
    | <circumfix>
    ]
}

token variable {
    | <sigil> <twigil>? <desigilname=ident>
    | <sigil> <?[<[]> <postcircumfix>
    | $<sigil>=['$'] $<desigilname>=[<[/_!]>]
}

token sigil { <[$@%&]> }

token twigil { <[*!?]> }

proto token package_declarator { <...> }
token package_declarator:sym<module> {
    :my $*PKGDECL := 'module';
    <sym> <package_def>
}
token package_declarator:sym<class> {
    :my $*PKGDECL := 'class';
    <sym> <package_def>
}
token package_declarator:sym<grammar> {
    :my $*PKGDECL := 'grammar';
    <sym> <package_def>
}
token package_declarator:sym<role> {
    :my $*PKGDECL := 'role';
    <sym> <package_def>
}

rule package_def { 
    :my $*IN_DECL := 'package';
    <name>
    [ 'is' <parent=name> ]?
    {*} #= open
    [ 
    || ';' <comp_unit>
    || <?[{]> <block>
    || <.panic: 'Malformed package declaration'>
    ]
}

token declarator {
    [
#    | <constant_declarator>
    | <variable_declarator>
#    | '(' ~ ')' <signature> <trait>*
    | <routine_declarator>
    | <regex_declarator>
#    | <type_declarator>
    ]
}

proto token multi_declarator { <...> }
token multi_declarator:sym<multi> {
    :my $*MULTINESS := 'multi';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed multi'> ]
}
token multi_declarator:sym<proto> {
    :my $*MULTINESS := 'proto';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed proto'> ]
}
token multi_declarator:sym<only> {
    :my $*MULTINESS := 'only';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed only'> ]
}
token multi_declarator:sym<null> {
    :my $*MULTINESS := '';
    <declarator>
}

proto token scope_declarator { <...> }
token scope_declarator:sym<my>  { <sym> <scoped('my')> }
token scope_declarator:sym<our> { <sym> <scoped('our')> }
token scope_declarator:sym<has> { <sym> <scoped('has')> }

rule scoped($*SCOPE) {
    | <variable_declarator>
    | <routine_declarator>
}

token variable_declarator { <variable> }

proto token routine_declarator { <...> }
token routine_declarator:sym<sub>    { <sym> <routine_def> }
token routine_declarator:sym<method> { <sym> <method_def> }

rule routine_def {
    <deflongname>?
    <.newpad>
    [ '(' <signature> ')' ]?
    <trait>*
    <blockoid>
}

rule method_def {
    <deflongname>?
    <.newpad>
    [ '(' <signature> ')' ]?
    <blockoid>
}


###########################
# Captures and Signatures #
###########################

rule param_sep { [','|':'|';'|';;'] }

token signature {
    :my $*IN_DECL := 'sig';
    :my $*zone := 'posreq';
    <.ws>
    [
    | <?before '-->' | ')' | ']' | '{' | ':'\s >
    | [ <parameter> || <.panic: 'Malformed parameter'> ]
    ] ** <param_sep>
    <.ws>
    { $*IN_DECL := ''; }
    [ '-->' <.ws> <typename> ]?
}

token parameter {
    :my $*PARAMETER := Perl6::Compiler::Parameter.new();
    [
    | <type_constraint>+
        [
        | $<quant>=['*'] <param_var>
        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
        ]
    | $<quant>=['*'] <param_var>
    | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
    | <longname> <.panic('Invalid typename in parameter declaration')>
    ]
    <post_constraint>*
    <default_value>?
}

token param_var { 
    <sigil> <twigil>?
    [ <name=ident> | $<name>=[<[/!]>] ]
}

token named_param {
    ':'
    [
    | <name=identifier> '(' <.ws>
        [ <named_param> | <param_var> <.ws> ]
        [ ')' || <.panic: 'Unable to parse named parameter; couldnt find right parenthesis'> ]
    | <param_var>
    ]
}

rule default_value {
    :my $*IN_DECL := '';
    '=' <EXPR('i=')>
}

token type_constraint {
    :my $*IN_DECL := '';
    [
    | <value>
    | <typename>
    | where <.ws> <EXPR('m=')>
    ]
    <.ws>
}

rule post_constraint {
    :my $*IN_DECL := '';
#    :dba('constraint')
    [
    | '[' ~ ']' <signature>
    | '(' ~ ')' <signature>
    | where <EXPR('m=')>
    ]
}

rule regex_declarator {
    [
    | $<proto>=[proto] [regex|token|rule] 
      <deflongname> 
      '{' '<...>' '}'<?ENDSTMT>
    | $<sym>=[regex|token|rule]
      <deflongname>
      <.newpad>
      [ '(' <signature> ')' ]?
      {*} #= open
      '{'<p6regex=LANG('Regex','nibbler')>'}'<?ENDSTMT>
    ]
}

rule trait {
    [
    | <trait_mod>
    | <colonpair>
    ]
}

proto token trait_mod { <...> }

token trait_mod:sym<is> {
    <sym>:s <longname><circumfix>?
}

# XXX This should elsewhere in this file
token dotty {
    '.' <identifier>
    [ 
    | <?[(]> <args>
    | ':' \s <args=arglist>
    ]?
}

## Nouns    

proto token term { <...> }

token term:sym<self> { <sym> » }

token term:sym<identifier> {
    <identifier> <?[(]> <args>
}

token term:sym<name> {
    <name> 
    [
    ||  <?{
            $/.CURSOR.is_name($<name>.Str)
        }>
    || <args>
    ]
}

token term:sym<pir::op> {
    'pir::' $<op>=[\w+] <args>?
}

token args {
    | '(' <semiarglist> ')'
    | [ \s <arglist> ]
    | <?>
}

token semiarglist {
    <arglist>
}

token arglist {
    <.ws> 
    [ 
    | <EXPR('f=')>
    | <?>
    ]
}


token term:sym<value> { <value> }

token value {
    | <integer>
    | <quote>
}

token typename {
    [
    | '::?'<identifier>                 # parse ::?CLASS as special case
    | <longname>
      <?{
        my $longname := $<longname>.Str;
        if pir::substr($longname, 0, 2) eq '::' {
            $/.CURSOR.add_my_name(pir::substr($longname, 2));
        }
        else {
            $/.CURSOR.is_name($longname);
        }
      }>
    ]
    # parametric type?
#    <.unsp>? [ <?before '['> <postcircumfix> ]?
#    <.ws> [ 'of' <.ws> <typename> ]?
}

proto token quote { <...> }
token quote:sym<apos> { <?[']>            <quote_EXPR: ':q'>  }
token quote:sym<dblq> { <?["]>            <quote_EXPR: ':qq'> }
token quote:sym<q>    { 'q'  <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qq>   { 'qq' <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<Q>    { 'Q'  <![(]> <.ws> <quote_EXPR> }
token quote:sym<Q:PIR> { 'Q:PIR' <.ws> <quote_EXPR> }

token quote_escape:sym<$>   { <?[$]> <?quotemod_check('s')> <variable> }

token circumfix:sym<( )> { '(' <.ws> <EXPR> ')' }
token circumfix:sym<[ ]> { '[' <.ws> <EXPR> ']' }
token circumfix:sym<ang> { <?[<]>  <quote_EXPR: ':q', ':w'>  }
token circumfix:sym<{ }> { <?[{]> <pblock(1)> }
token circumfix:sym<sigil> { <sigil> '(' ~ ')' <semilist> }

rule semilist { <statement> }

## Operators

INIT {
    Perl6::Grammar.O(':prec<y=>, :assoc<unary>', '%methodop');
    Perl6::Grammar.O(':prec<x=>, :assoc<unary>', '%autoincrement');
    Perl6::Grammar.O(':prec<w=>, :assoc<left>',  '%exponentiation');
    Perl6::Grammar.O(':prec<v=>, :assoc<unary>', '%symbolic_unary');
    Perl6::Grammar.O(':prec<u=>, :assoc<left>',  '%multiplicative');
    Perl6::Grammar.O(':prec<t=>, :assoc<left>',  '%additive');
    Perl6::Grammar.O(':prec<s=>, :assoc<left>',  '%replication');
    Perl6::Grammar.O(':prec<r=>, :assoc<left>',  '%concatenation');
    Perl6::Grammar.O(':prec<o=>, :assoc<unary>', '%named_unary');
    Perl6::Grammar.O(':prec<m=>, :assoc<left>, :pasttype<chain>',  '%chaining');
    Perl6::Grammar.O(':prec<l=>, :assoc<left>',  '%tight_and');
    Perl6::Grammar.O(':prec<k=>, :assoc<left>',  '%tight_or');
    Perl6::Grammar.O(':prec<j=>, :assoc<right>', '%conditional');
    Perl6::Grammar.O(':prec<i=>, :assoc<right>', '%item_assignment');
    Perl6::Grammar.O(':prec<g=>, :assoc<list>, :nextterm<nulltermish>',  '%comma');
    Perl6::Grammar.O(':prec<f=>, :assoc<list>',  '%list_infix');
    Perl6::Grammar.O(':prec<e=>, :assoc<right>', '%list_assignment');
    Perl6::Grammar.O(':prec<d=>, :assoc<left>',  '%loose_and');
    Perl6::Grammar.O(':prec<c=>, :assoc<left>',  '%loose_or');
}


token nulltermish { 
    | <OPER=term=termish> 
    | <?>
}

token infixish {
    | <OPER=infix=infix> <![=]>
    | <infix> <OPER=infix_postfix_meta_operator=infix_postfix_meta_operator>
}

proto token infix_postfix_meta_operator { <...> }

token postcircumfix:sym<[ ]> { 
    '[' <.ws> <EXPR> ']' 
    <O('%methodop')>
}

token postcircumfix:sym<{ }> {
    '{' <.ws> <EXPR> '}'
    <O('%methodop')>
}

token postcircumfix:sym<ang> {
    <?[<]> <quote_EXPR: ':q'>
    <O('%methodop')>
}

token postcircumfix:sym<( )> { 
    '(' <.ws> <arglist> ')' 
    <O('%methodop')>
}

token postfix:sym<.>  { <dotty> <O('%methodop')> }

token prefix:sym<++>  { <sym>  <O('%autoincrement')> }
token prefix:sym<-->  { <sym>  <O('%autoincrement')> }
token postfix:sym<++> { <sym>  <O('%autoincrement')> }
token postfix:sym<--> { <sym>  <O('%autoincrement')> }

token infix:sym<**>   { <sym>  <O('%exponentiation')> }

token prefix:sym<+>   { <sym>  <O('%symbolic_unary, :pirop<set N*>')> }
token prefix:sym<~>   { <sym>  <O('%symbolic_unary')> }
token prefix:sym<->   { <sym>  <O('%symbolic_unary, :pirop<neg>')> }
token prefix:sym<?>   { <sym>  <O('%symbolic_unary, :pirop<istrue>')> }
token prefix:sym<!>   { <sym>  <O('%symbolic_unary, :pirop<isfalse>')> }
token prefix:sym<+^>  { <sym>  <O('%symbolic_unary, :pirop<bnot>')> }

token infix:sym<*>    { <sym>  <O('%multiplicative')> }
token infix:sym</>    { <sym>  <O('%multiplicative')> }
token infix:sym<%>    { <sym>  <O('%multiplicative')> }
token infix:sym<+&>   { <sym>  <O('%multiplicative')> }
token infix:sym<~&>   { <sym>  <O('%multiplicative')> }

token infix:sym<+>    { <sym>  <O('%additive')> }
token infix:sym<->    { <sym>  <O('%additive')> }
token infix:sym<+|>   { <sym>  <O('%additive')> }
token infix:sym<+^>   { <sym>  <O('%additive')> }
token infix:sym<~|>   { <sym>  <O('%additive')> }
token infix:sym<~^>   { <sym>  <O('%additive')> }
token infix:sym«+<»   { <sym>  <O('%additive')> }
token infix:sym«+>»   { <sym>  <O('%additive')> }

token infix:sym<x>    { <sym>  <O('%replication')> }

token infix:sym<~>    { <sym>  <O('%concatenation , :pirop<concat>')> }

token prefix:sym<abs> { <sym> » <O('%named_unary, :pirop<abs PP>')> }

token infix:sym«==»   { <sym>  <O('%chaining')> }
token infix:sym«!=»   { <sym>  <O('%chaining')> }
token infix:sym«<=»   { <sym>  <O('%chaining')> }
token infix:sym«>=»   { <sym>  <O('%chaining')> }
token infix:sym«<»    { <sym>  <O('%chaining')> }
token infix:sym«>»    { <sym>  <O('%chaining')> }
token infix:sym«eq»   { <sym>  <O('%chaining')> }
token infix:sym«ne»   { <sym>  <O('%chaining')> }
token infix:sym«le»   { <sym>  <O('%chaining')> }
token infix:sym«ge»   { <sym>  <O('%chaining')> }
token infix:sym«lt»   { <sym>  <O('%chaining')> }
token infix:sym«gt»   { <sym>  <O('%chaining')> }
token infix:sym«=:=»  { <sym>  <O('%chaining')> }
token infix:sym<eqv>  { <sym>  <O('%chaining')> }

token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

token infix:sym<||>   { <sym>  <O('%tight_or, :pasttype<unless>')> }
token infix:sym<^^>   { <sym>  <O('%tight_or, :pasttype<xor>')> }
token infix:sym<//>   { <sym>  <O('%tight_or, :pasttype<def_or>')> }

token infix:sym<?? !!> { 
    '??'
    <.ws>
    <EXPR('i=')>
    '!!'
    <O('%conditional, :reducecheck<ternary>, :pasttype<if>')> 
}

#token infix:sym<:=>   { <sym>  <O('%assignment, :pasttype<bind>')> }
#token infix:sym<::=>  { <sym>  <O('%assignment, :pasttype<bind>')> }

token infix_postfix_meta_operator:sym<=> { '=' <O('%item_assignment')> }

token infix:sym<,>    { <sym>  <O('%comma')> }

token infix:sym<Z>    { <sym>  <O('%list_infix')> }

token infix:sym<=>    { <sym>  <O('%list_assignment')> }

token infix:sym<and>  { <sym>  <O('%loose_and, :pasttype<if>')> }

token infix:sym<or>   { <sym>  <O('%loose_or, :pasttype<unless>')> }
token infix:sym<xor>  { <sym>  <O('%loose_or, :pasttype<xor>')> }
token infix:sym<err>  { <sym>  <O('%loose_or, :pasttype<def_or>')> }

grammar Perl6::Regex is Regex::P6Regex::Grammar {
    token metachar:sym<:my> { 
        ':' <?before 'my'> <statement=LANG('MAIN', 'statement')> <.ws> ';' 
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token codeblock {
        <block=LANG('MAIN','block')>
    }
}
