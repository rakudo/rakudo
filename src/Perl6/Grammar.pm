grammar Perl6::Grammar is HLL::Grammar;


method TOP() {
    my %*LANG;
    %*LANG<Regex>         := Perl6::Regex;
    %*LANG<Regex-actions> := Perl6::RegexActions;
    %*LANG<MAIN>          := Perl6::Grammar;
    %*LANG<MAIN-actions>  := Perl6::Actions;
    my $*SCOPE := '';
    self.comp_unit;
}

## Lexer stuff

token identifier { <ident> }

token name { <identifier> ** '::' }

token deflongname { 
    <identifier> 
    [ ':sym<' $<sym>=[<-[>]>*] '>' | ':sym«' $<sym>=[<-[»]>*] '»' ]?
}

token ENDSTMT {
    [ \h* $$ <.ws> <?MARKER('endstmt')> ]?
}

## Top-level rules

token comp_unit { 
    <.newpad>
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

token xblock {
    <EXPR> <.ws> <pblock>
}

token pblock {
    <?[{]> 
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
    <xblock>
}

token statement_control:sym<return> {
    <sym> :s 
    [ <EXPR> || <.panic: 'return requires an expression argument'> ]
}

token statement_control:sym<make> {
    <sym> :s 
    [ <EXPR> || <.panic: 'make requires an expression argument'> ]
}

proto token statement_prefix { <...> }
token statement_prefix:sym<INIT> { <sym> <blorst> }

token blorst {
    \s <.ws> [ <pblock> | <statement> ]
}

## Terms

token term:sym<colonpair>          { <colonpair> }
token term:sym<variable>           { <variable> }
token term:sym<package_declarator> { <package_declarator> }
token term:sym<scope_declarator>   { <scope_declarator> }
token term:sym<routine_declarator> { <routine_declarator> }
token term:sym<regex_declarator>   { <regex_declarator> }
token term:sym<statement_prefix>   { <statement_prefix> }

token colonpair {
    ':' 
    [ 
    | $<not>='!' <identifier>
    | <identifier> <circumfix>?
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
token package_declarator:sym<module> { <sym> <package_def> }
token package_declarator:sym<class>  { $<sym>=[class|grammar] <package_def> }

rule package_def { 
    <name> 
    [ 'is' <parent=name> ]? 
    [ 
    || ';' <comp_unit>
    || <?[{]> <pblock>
    || <.panic: 'Malformed package declaration'>
    ]
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
    [ '(' <signature> ')'
        || <.panic: 'Routine declaration requires a signature'> ]
    <blockoid>
}

rule method_def {
    <deflongname>?
    <.newpad>
    [ '(' <signature> ')'
        || <.panic: 'Routine declaration requires a signature'> ]
    <blockoid>
}

token signature { [ [<.ws><parameter><.ws>] ** ',' ]? }

token parameter { 
    [
    | $<quant>=['*'] <param_var>
    | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
    ]
    <default_value>?
}

token param_var { 
    <sigil> <twigil>?
    [ <name=ident> | $<name>=[<[/!]>] ]
}

token named_param {
    ':' <param_var>
}

rule default_value { '=' <EXPR('i=')> }

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

token dotty {
    '.' <identifier>
    [ 
    | <?[(]> <args>
    | ':' \s <args=arglist>
    ]?
}
    

proto token term { <...> }

token term:sym<self> { <sym> » }

token term:sym<identifier> {
    <identifier> <?[(]> <args>
}

token term:sym<name> {
    <name> <args>?
}

token term:sym<pir::op> {
    'pir::' $<op>=[\w+] <args>?
}

token args {
    | '(' <arglist> ')'
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

proto token quote { <...> }
token quote:sym<apos> { <?[']>            <quote_EXPR: ':q'>  }
token quote:sym<dblq> { <?["]>            <quote_EXPR: ':qq'> }
token quote:sym<q>    { 'q'  <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qq>   { 'qq' <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<Q>    { 'Q'  <![(]> <.ws> <quote_EXPR> }
token quote:sym<Q:PIR> { 'Q:PIR' <.ws> <quote_EXPR> }

token circumfix:sym<( )> { '(' <.ws> <EXPR> ')' }
token circumfix:sym<ang> { <?[<]>  <quote_EXPR: ':q', ':w'>  }
token circumfix:sym<{ }> { <?[{]> <pblock> }
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
    Perl6::Grammar.O(':prec<r=>, :assoc<left>',  '%concatenation');
    Perl6::Grammar.O(':prec<m=>, :assoc<left>',  '%relational');
    Perl6::Grammar.O(':prec<l=>, :assoc<left>',  '%tight_and');
    Perl6::Grammar.O(':prec<k=>, :assoc<left>',  '%tight_or');
    Perl6::Grammar.O(':prec<j=>, :assoc<right>', '%conditional');
    Perl6::Grammar.O(':prec<i=>, :assoc<right>', '%assignment');
    Perl6::Grammar.O(':prec<g=>, :assoc<list>, :nextterm<nulltermish>',  '%comma');
    Perl6::Grammar.O(':prec<f=>, :assoc<list>',  '%list_infix');
}


token nulltermish { 
    | <OPER=term=termish> 
    | <?>
}

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

token prefix:sym<++>  { <sym>  <O('%autoincrement, :pirop<inc>')> }
token prefix:sym<-->  { <sym>  <O('%autoincrement, :pirop<dec>')> }

# see Actions.pm for postfix:<++> and postfix:<-->
token postfix:sym<++> { <sym>  <O('%autoincrement')> }
token postfix:sym<--> { <sym>  <O('%autoincrement')> }

token infix:sym<**>   { <sym>  <O('%exponentiation, :pirop<pow>')> }

token prefix:sym<+>   { <sym>  <O('%symbolic_unary, :pirop<set N*>')> }
token prefix:sym<~>   { <sym>  <O('%symbolic_unary, :pirop<set S*>')> }
token prefix:sym<->   { <sym>  <O('%symbolic_unary, :pirop<neg>')> }
token prefix:sym<?>   { <sym>  <O('%symbolic_unary, :pirop<istrue>')> }
token prefix:sym<!>   { <sym>  <O('%symbolic_unary, :pirop<isfalse>')> }

token infix:sym<*>    { <sym>  <O('%multiplicative, :pirop<mul>')> }
token infix:sym</>    { <sym>  <O('%multiplicative, :pirop<div>')> }
token infix:sym<%>    { <sym>  <O('%multiplicative, :pirop<mod>')> }

token infix:sym<+>    { <sym>  <O('%additive, :pirop<add>')> }
token infix:sym<->    { <sym>  <O('%additive, :pirop<sub>')> }

token infix:sym<~>    { <sym>  <O('%concatenation , :pirop<concat>')> }

token infix:sym«==»   { <sym>  <O('%relational, :pirop<iseq INn>')> }
token infix:sym«!=»   { <sym>  <O('%relational, :pirop<isne INn>')> }
token infix:sym«<=»   { <sym>  <O('%relational, :pirop<isle INn>')> }
token infix:sym«>=»   { <sym>  <O('%relational, :pirop<isge INn>')> }
token infix:sym«<»    { <sym>  <O('%relational, :pirop<islt INn>')> }
token infix:sym«>»    { <sym>  <O('%relational, :pirop<isgt INn>')> }
token infix:sym«eq»   { <sym>  <O('%relational, :pirop<iseq ISs>')> }
token infix:sym«ne»   { <sym>  <O('%relational, :pirop<isne ISs>')> }
token infix:sym«le»   { <sym>  <O('%relational, :pirop<isle ISs>')> }
token infix:sym«ge»   { <sym>  <O('%relational, :pirop<isge ISs>')> }
token infix:sym«lt»   { <sym>  <O('%relational, :pirop<islt ISs>')> }
token infix:sym«gt»   { <sym>  <O('%relational, :pirop<isgt ISs>')> }
token infix:sym«=:=»  { <sym>  <O('%relational, :pirop<issame>')> }

token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

token infix:sym<||>   { <sym>  <O('%tight_or, :pasttype<unless>')> }
token infix:sym<//>   { <sym>  <O('%tight_or, :pasttype<def_or>')> }

token infix:sym<?? !!> { 
    '??'
    <.ws>
    <EXPR('i=')>
    '!!'
    <O('%conditional, :reducecheck<ternary>, :pasttype<if>')> 
}

token infix:sym<:=>   { <sym>  <O('%assignment, :pasttype<bind>')> }
token infix:sym<::=>  { <sym>  <O('%assignment, :pasttype<bind>')> }

token infix:sym<,>    { <sym>  <O('%comma, :pasttype<list>')> }


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
        <block=LANG('MAIN','pblock')>
    }
}
