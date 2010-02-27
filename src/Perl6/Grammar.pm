grammar Perl6::Grammar is HLL::Grammar;

our %COMPILINGPACKAGES;
our %STUBCOMPILINGPACKAGES;

INIT {
    our %COMPILINGPACKAGES := Q:PIR { %r = root_new ['parrot';'Hash'] };
    our %STUBCOMPILINGPACKAGES := Q:PIR { %r = root_new ['parrot';'Hash'] };
}

method TOP() {
    my %*LANG;
    %*LANG<Regex>         := Perl6::Regex;
    %*LANG<Regex-actions> := Perl6::RegexActions;
    %*LANG<MAIN>          := Perl6::Grammar;
    %*LANG<MAIN-actions>  := Perl6::Actions;
    my %*HOW;
    %*HOW<package> := 'none';
    %*HOW<module>  := 'none';
    %*HOW<class>   := 'ClassHOW';
    %*HOW<grammar> := 'GrammarHOW';
    %*HOW<role>    := 'RoleHOW';
    my %*PKGCOMPILER;
    %*PKGCOMPILER<role>    := Perl6::Compiler::Role;
    %*PKGCOMPILER<module>  := Perl6::Compiler::Module;
    %*PKGCOMPILER<package> := Perl6::Compiler::Module;
    my $*SCOPE := '';
    my $*MULTINESS := '';
    my $*TYPENAME := '';
    self.comp_unit;
}

method add_my_name($name) {
    my @BLOCK := Q:PIR{ %r = get_hll_global ['Perl6';'Actions'], '@BLOCK' };
    
    # We need to flag up most re-declaration collisions.
    my $cur_decl := @BLOCK[0].symbol($name);
    if $cur_decl {
        if $*PKGDECL eq 'role' || $cur_decl<stub> {
            return 1;
        }
        else {
            pir::die("Illegal redeclaration of symbol '$name'");
        }
    }

    # Add it.
    @BLOCK[0].symbol($name, :does_abstraction(1));
    return 1;
}

method add_our_name($name) {
    our %COMPILINGPACKAGES;
    our %STUBCOMPILINGPACKAGES;

    # Check if it already exists, if we care.
    if $*PKGDECL ne 'role' {
        my $exists := 0;
        if %STUBCOMPILINGPACKAGES{$name} {
            %STUBCOMPILINGPACKAGES{$name} := 0;
        }
        elsif %COMPILINGPACKAGES{$name} {
            $exists := 1;
        }
        else {
            my @parts := parse_name($name);
            my $final := pir::pop(@parts);
            my $test  := pir::get_hll_global__PPS(@parts, $final);
            $exists   := !pir::isnull__IP($test);
        }
        if $exists {
            pir::die("Illegal redeclaration of symbol '$name'");
        }
    }

    # For now, just add a marker to say we saw this. We'll most likely want
    # to do something more here later (STD does quite a bit more, but we'll
    # build our way towards it).
    %COMPILINGPACKAGES{$name} := 1;

    # Always need to add our names as lexical names too.
    return self.add_my_name($name);
}

method add_name($name) {
    if $*SCOPE eq 'augment' || $*SCOPE eq 'supersede' {
        unless self.is_name($name) {
            pir::die("Can't $*SCOPE something that doesn't exist");
        }
    }
    else {
        if $*SCOPE eq 'our' {
            self.add_our_name($name);
        }
        else {
            self.add_my_name($name);
        }
    }
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
    my $test  := pir::get_hll_global__PPs(@parts, $final);
    !pir::isnull__IP($test);
}

# "when" arg assumes more things will become obsolete after Perl 6 comes out...
method obs ($old, $new, $when = ' in Perl 6') {
    self.panic("Unsupported use of $old;$when please use $new");
}

## Lexer stuff

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
    '::' <identifier>
}

token longname {
    <name> <colonpair>*
}

token deflongname { 
    <name> <colonpair>*
}

token module_name {
    <longname>
    [ <?before '['> '[' ~ ']' <arglist> ]?
}

token def_module_name {
    <longname>
    [
        <?before '['>
        <?{ $*PKGDECL eq 'role' }>
        :my $*SCOPE := 'my';
        '[' ~ ']' <signature>
    ]?
}

token nofun { <![ ( \\ ' \- ]> » }

token ENDSTMT {
    [ 
    | \h* $$ <.ws> <?MARKER('endstmt')> 
    | <.unv>? $$ <.ws> <?MARKER('endstmt')>
    ]?
}

token unsp {
    \\ <?before [\s|'#'] >
    # :dba('unspace')
    [
    | <.vws>
    | <.unv>
    ]*
}

token vws {
    #:dba('vertical whitespace')
    \v
}

token ws { 
    ||  <?MARKED('ws')> 
    ||  <!ww>
        [ \s+
        | '#' \N*
        | ^^ <.pod_comment>
        ]*
        <?MARKER('ws')>
}

token unv {
    # :dba('horizontal whitespace')
    [
    | ^^ <?before \h* '=' [ \w | '\\'] > <.pod_comment>
    | \h* '#' \N*
    | \h+
    ]
}


token pod_comment {
    ^^ \h* '=' 
    [
    | 'begin' \h+ 'END' >>
        [ .*? \n '=' 'end' \h+ 'END' » \N* || .* ]
    | 'begin' \h+ <identifier>
        [
        ||  .*? \n '=' 'end' \h+ $<identifier> » \N*
        ||  <.panic: '=begin without matching =end'>
        ]
    | 'begin' » \h* 
        [ $$ || '#' || <.panic: 'Unrecognized token after =begin'> ]
        [ 
        || .*? \n \h* '=' 'end' » \N*
        || <.panic: '=begin without matching =end'> 
        ]
    | 
        [ <?before .*? ^^ '=cut' » > 
          <.panic: 'Obsolete pod format, please use =begin/=end instead'> ]?
        [ <alpha> || \s || <.panic: 'Illegal pod directive'> ]
        \N*
    ]
}

## Top-level rules

token comp_unit {
    :my %*METAOPGEN;                           # hash of generated metaops
    :my $*IN_DECL;                             # what declaration we're in
    :my $*IMPLICIT;                            # whether we allow an implicit param
    <.newpad>
    <.outerlex>
    <.finishpad>
    <statementlist> 
    [ $ || <.panic: 'Confused'> ] 
}

rule statementlist {
    | $
    | [<statement><.eat_terminator> ]*
}

rule semilist {
    | <?[ ) \] } ]>
    | [<statement><.eat_terminator> ]*
}

token statement {
    <!before <[\])}]> | $ >
    [
    | <statement_control>
    | <EXPR> <.ws>
        [
        || <?MARKED('endstmt')>
        || <statement_mod_cond> <statement_mod_loop>?
        || <statement_mod_loop>
        ]?
    ]
    | <?before ';'>
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
    | <lambda>
        <.newpad>
        :my $*SCOPE := 'my';
        <signature>
        <blockoid>
    | <?[{]> 
        <.newpad>
        <blockoid>
    | <.panic: 'Missing block'>
}

token lambda { '->' | '<->' }

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
token outerlex { <?> }
token finishpad { <?> }

proto token terminator { <...> }

token terminator:sym<;> { <?[;]> }
token terminator:sym<)> { <?[)]> }
token terminator:sym<]> { <?[\]]> }
token terminator:sym<}> { <?[}]> }
token terminator:sym<if>     { 'if'     <.nofun> }
token terminator:sym<unless> { 'unless' <.nofun> }
token terminator:sym<while>  { 'while'  <.nofun> }
token terminator:sym<until>  { 'until'  <.nofun> }
token terminator:sym<for>    { 'for'    <.nofun> }
token terminator:sym<given>  { 'given'  <.nofun> }
token terminator:sym<when>   { 'when'   <.nofun> }

token stdstopper { <?terminator> }

## Statement control

proto token statement_control { <...> }

token statement_control:sym<if> {
    <sym> :s
    <xblock>
    [ 'elsif'\s <xblock> ]*
    [ 'else'\s <else=.pblock> ]?
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
    [ <?before 'my'? '$'\w+ '(' >
        <.panic: "This appears to be Perl 5 code"> ]?
    [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
        <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
    <xblock(1)>
}

token statement_control:sym<loop> {
    <sym>
    [ <?[({]> <.panic: "Whitespace required after 'loop'"> ]?
    :s
    [ '('
        <e1=.EXPR>? ';'
        <e2=.EXPR>? ';'
        <e3=.EXPR>?
    ')' ]?
    <block>
}

token statement_control:sym<return> {
    <sym> :s <EXPR>?
}

token statement_control:sym<use> {
    <sym> <.ws> 
    [
    | 'v6'
    | <module_name=.longname>
    ]
}

token statement_control:sym<given> {
    <sym> :s <xblock(1)>
}
token statement_control:sym<when> {
    <sym> :s <xblock>
}
rule statement_control:sym<default> {
    <sym> <block>
}

rule statement_control:sym<CATCH> {<sym> <block(1)> }
rule statement_control:sym<CONTROL> {<sym> <block(1)> }

proto token statement_prefix { <...> }
token statement_prefix:sym<BEGIN> { <sym> <blorst> }
token statement_prefix:sym<CHECK> { <sym> <blorst> }
token statement_prefix:sym<INIT>  { <sym> <blorst> }
token statement_prefix:sym<END>   { <sym> <blorst> }
token statement_prefix:sym<try>   { <sym> <blorst> }
token statement_prefix:sym<gather>{ <sym> <blorst> }
token statement_prefix:sym<do>    { <sym> <blorst> }

token blorst {
    \s <.ws> [ <?[{]> <block> | <statement> ]
}

## Statement modifiers

proto token statement_mod_cond { <...> }

token statement_mod_cond:sym<if>     { <sym> :s <cond=.EXPR> }
token statement_mod_cond:sym<unless> { <sym> :s <cond=.EXPR> }

proto token statement_mod_loop { <...> }

token statement_mod_loop:sym<while> { <sym> :s <smexpr=.EXPR> }
token statement_mod_loop:sym<until> { <sym> :s <smexpr=.EXPR> }
token statement_mod_loop:sym<for>   { <sym> :s <smexpr=.EXPR> }

## Terms

token term:sym<fatarrow>           { <fatarrow> }
token term:sym<colonpair>          { <colonpair> }
token term:sym<variable>           { <variable> }
token term:sym<package_declarator> { <package_declarator> }
token term:sym<scope_declarator>   { <scope_declarator> }
token term:sym<routine_declarator> { <routine_declarator> }
token term:sym<multi_declarator>   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
token term:sym<regex_declarator>   { <regex_declarator> }
token term:sym<statement_prefix>   { <statement_prefix> }
token term:sym<*>                  { <sym> }
token term:sym<lambda>             { <?lambda> <pblock> }

token term:sym<undef> {
    <sym> >> {}
    [ <?before \h*'$/' >
        <.obs('$/ variable as input record separator',
             "the filehandle's .slurp method")>
    ]?
    [ <?before [ '(' || \h*<sigil><twigil>?\w ] >
        <.obs('undef as a verb', 'undefine function or assignment of Nil')>
    ]?
    <.obs('undef as a value', "something more specific:\n\tMu (the \"most undefined\" type object),\n\tan undefined type object such as Int,\n\tNil as an empty list,\n\t*.notdef as a matcher or method,\n\tAny:U as a type constraint\n\tor fail() as a failure return\n\t   ")>
}

token fatarrow {
    <key=.identifier> \h* '=>' <.ws> <val=.EXPR('i=')>
}

token colonpair {
    :my $*key;
    :my $*value;

    ':'
    [
    | '!' <identifier> [ <[ \[ \( \< \{ ]> <.panic: "Argument not allowed on negated pair"> ]?
        { $*key := $<identifier>.Str; $*value := 0; }
    | <identifier>
        { $*key := $<identifier>.Str; }
        [
        || <.unsp>? <circumfix> { $*value := $<circumfix>; }
        || { $*value := 1; }
        ]
    | <circumfix>
        { $*key := ""; $*value := $<circumfix>; }
    | $<var> = (<sigil> {} <twigil>? <desigilname>)
        { $*key := $<var><desigilname>.Str; $*value := $<var>; }
    ]
}

token desigilname {
    [
#    | <?before '$' > <variable>
    | <longname>
    ]
}

token variable {
    | <sigil> <twigil>? <desigilname>
    | <sigil> $<index>=[\d+]
    | <sigil> <?[<[]> <postcircumfix>
    | $<sigil>=['$'] $<desigilname>=[<[/_!]>]
}

token sigil { <[$@%&]> }

proto token twigil { <...> }
token twigil:sym<.> { <sym> }
token twigil:sym<!> { <sym> }
token twigil:sym<^> { <sym> <?before \w> }
token twigil:sym<:> { <sym> <?before \w> }
token twigil:sym<*> { <sym> }
token twigil:sym<?> { <sym> }

proto token package_declarator { <...> }
token package_declarator:sym<package> {
    :my $*PKGDECL := 'package';
    <sym> <package_def>
}
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

token package_declarator:sym<does> {
    <sym> <.ws>
    <typename>
}

rule package_def { 
    :my $*IN_DECL := 'package';
    <def_module_name>?
    <trait>*
    {*} #= open
    [ 
    || ';' 
        <.newpad>
        <.finishpad>
        <statementlist> 
    || <?[{]> <block>
    || <.panic: 'Malformed package declaration'>
    ]
}

token declarator {
    [
#    | <constant_declarator>
    | <variable_declarator>
    | '(' ~ ')' <signature> <trait>*
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
token scope_declarator:sym<my>        { <sym> <scoped('my')> }
token scope_declarator:sym<our>       { <sym> <scoped('our')> }
token scope_declarator:sym<has>       { <sym> <scoped('has')> }
token scope_declarator:sym<augment>   { <sym> <scoped('augment')> }
token scope_declarator:sym<supersede> { 
    <sym> <.panic: '"supersede" not yet implemented'>
}

rule scoped($*SCOPE) {
    :my $*TYPENAME := '';
    [
    | <DECL=variable_declarator>
    | <DECL=routine_declarator>
    | <DECL=package_declarator>
    | <typename>+ 
      {
        if +$<typename> > 1 {
            $/.CURSOR.panic("Multiple prefix constraints not yet supported");
        }
        $*TYPENAME := $<typename>[0].ast;
      }
      <DECL=multi_declarator>
    | <DECL=multi_declarator>
    ]
    || <?before <[A..Z]>><longname>{
            my $t := $<longname>.Str;
            $/.CURSOR.panic("In \"$*SCOPE\" declaration, typename $t must be predeclared (or marked as declarative with :: prefix)");
        }
        <!> # drop through
    || { $/.CURSOR.panic("Malformed $*SCOPE") }
}

token variable_declarator {
    :my $*IN_DECL := 'variable';
    <variable>
    { $*IN_DECL := '' }
    [
        <.unsp>? 
        $<shape>=[
        | '(' ~ ')' <signature>
        | '[' ~ ']' <semilist>
        | '{' ~ '}' <semilist>
        ]*
    ]?
    <trait>*
}

proto token routine_declarator { <...> }
token routine_declarator:sym<sub>       
    { <sym> <.nofun> <routine_def> }
token routine_declarator:sym<method>    
    { <sym> <.nofun> :my $*METHODTYPE := 'Method'; <method_def> }
token routine_declarator:sym<submethod> 
    { <sym> <.nofun> :my $*METHODTYPE := 'Submethod'; <method_def> }

rule routine_def {
    :my $*IN_DECL := 'routine';
    <deflongname>?
    <.newpad>
    [ '(' <multisig> ')' ]?
    <trait>*
    { $*IN_DECL := ''; }
    <blockoid>
}

rule method_def {
    :my $*IN_DECL := 'method';
    [
        <.newpad>
        [
            | $<specials>=[<[ ! ^ ]>?]<longname> [ '(' <multisig> ')' ]? <trait>*
            | [ '(' <multisig> ')' ]? <trait>*
            | <?>
        ]
        { $*IN_DECL := ''; }
        <blockoid>
    ] || <.panic: 'Malformed method'>
}


###########################
# Captures and Signatures #
###########################

token capterm {
    '\\'
    [
    | '(' <capture>? ')'
    | <?before \S> <termish>
    | {} <.panic: "You can't backslash that">
    ]
}

rule capture {
    <EXPR>
}

rule param_sep {
    $<sep>=[','|':'|';;'|';'] { @*seps.push($<sep>) }
}

# XXX Not really implemented yet.
rule multisig {
    :my $*SCOPE := 'my';
    <signature>
}

token signature {
    :my $*IN_DECL := 'sig';
    :my $*zone := 'posreq';
    :my @*seps := pir::new__PS('ResizablePMCArray');
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
        | $<quant>=['*'|'\\'|'|'] <param_var>
        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
        | <?>
        ]
    | $<quant>=['*'|'\\'|'|'] <param_var>
    | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
    | <longname> <.panic('Invalid typename in parameter declaration')>
    ]
    <trait>*
    <post_constraint>*
    <default_value>?
}

token param_var {
    | '[' ~ ']' <signature>
    | '(' ~ ')' <signature>
    | <sigil> <twigil>?
      [
      || <name=.identifier>
      || $<name>=[<[/!]>]
      ]?
}

token named_param {
    ':'
    [
    | <name=.identifier> '(' <.ws>
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
      '{'<p6regex=.LANG('Regex','nibbler')>'}'<?ENDSTMT>
    ]
}

proto token type_declarator { <...> }

token type_declarator:sym<enum> {
    <sym> <.ws>
    <name>? <.ws>
    <?before '(' | '<' | '<<' | '«' > <circumfix>
}

rule trait {
    :my $*IN_DECL := '';
    [
    | <trait_mod>
    | <colonpair>
    ]
}

proto token trait_mod { <...> }
token trait_mod:sym<is>      { <sym>:s <longname><circumfix>? }
token trait_mod:sym<hides>   { <sym>:s <module_name> }
token trait_mod:sym<does>    { <sym>:s <module_name> }
token trait_mod:sym<will>    { <sym>:s <identifier> <pblock> }
token trait_mod:sym<of>      { <sym>:s <typename> }
token trait_mod:sym<as>      { <sym>:s <typename> }
token trait_mod:sym<returns> { <sym>:s <typename> }
token trait_mod:sym<handles> { <sym>:s <term> }


## Terms

proto token term { <...> }

token term:sym<self> { <sym> <.nofun> }

token term:sym<Nil>  { <sym> <.nofun> }
token term:sym<rand> {
    <sym> »
    [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand or (1..N).pick')> ]?
    [ <?before '()'> <.obs('rand()', 'rand')> ]?
}

token term:sym<...> { <sym> <args>? }
token term:sym<???> { <sym> <args>? }
token term:sym<!!!> { <sym> <args>? }

token term:sym<identifier> {
    <identifier> <?[(]> <args>
}

token term:sym<name> {
    <longname> 
    [
    ||  <?{
            my $longname := $<longname>.Str;
            pir::substr($longname, 0, 2) eq '::' || $/.CURSOR.is_name($longname)
        }>
    || <args>
    ]
}

token term:sym<pir::op> {
    'pir::' $<op>=[\w+] <args>?
}

token term:sym<dotty> { <dotty> }

token term:sym<capterm> { <capterm> }

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
    | <?stdstopper>
    | <EXPR('e=')>
    | <?>
    ]
}


token term:sym<value> { <value> }

proto token value { <...> }
token value:sym<quote>  { <quote> }
token value:sym<number> { <number> }

proto token number { <...> }
token number:sym<rational> { <nu=.integer>'/'<de=.integer> }
token number:sym<complex>  { <im=.numish>'\\'?'i' }
token number:sym<numish>   { <numish> }

token numish {
    [
    | <dec_number>
    | <integer>
#    | <rad_number>
    | 'NaN' >>
    | 'Inf' >>
    | '+Inf' >>
    | '-Inf' >>
    ]
}

token dec_number {
    | $<coeff> = [               '.' <frac=.decint> ] <escale>?
    | $<coeff> = [ <int=.decint> '.' <frac=.decint> ] <escale>?
    | $<coeff> = [ <int=.decint>                    ] <escale>
}

token escale { <[Ee]> $<sign>=[<[+\-]>?] <decint> }

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
    [<.ws> 'of' <.ws> <typename> ]?
}

token term:sym<type_declarator>   { <type_declarator> }

proto token quote { <...> }
token quote:sym<apos>  { <?[']>             <quote_EXPR: ':q'>  }
token quote:sym<dblq>  { <?["]>             <quote_EXPR: ':qq'> }
token quote:sym<q>     { 'q'   <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qq>    { 'qq'  <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<qx>    { 'qx'  <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qqx>   { 'qqx' <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<Q>     { 'Q'   <![(]> <.ws> <quote_EXPR> }
token quote:sym<Q:PIR> { 'Q:PIR'      <.ws> <quote_EXPR> }
token quote:sym</null/> { '/' \s* '/' <.panic: "Null regex not allowed"> }
token quote:sym</ />  { '/'<p6regex=.LANG('Regex','nibbler')>'/' }
token quote:sym<rx>   { <sym> >> '/'<p6regex=.LANG('Regex','nibbler')>'/' }
token quote:sym<m> {
    <sym> >>
    [
    | '/'<p6regex=.LANG('Regex','nibbler')>'/'
    | '{'<p6regex=.LANG('Regex','nibbler')>'}'
    ]
}


token quote_escape:sym<$>   { <?[$]> <?quotemod_check('s')> <variable> }
token quote_escape:sym<{ }> { <?[{]> <?quotemod_check('c')> <block> }

token circumfix:sym<( )> { '(' <semilist> ')' }
token circumfix:sym<[ ]> { '[' <semilist> ']' }
token circumfix:sym<ang> {
    <?[<]>
    [ <?before '<STDIN>' > <.obs('<STDIN>', '$*IN.lines')> ]?
    [ <?before '<>' > <.obs('<>', 'lines() or ()')> ]?
    <quote_EXPR: ':q', ':w'>
}
token circumfix:sym<« »> { <?[«]>  <quote_EXPR: ':qq', ':w'> }
token circumfix:sym<{ }> { <?[{]> <pblock(1)> }
token circumfix:sym<sigil> { <sigil> '(' ~ ')' <semilist> }

## Operators

INIT {
    Perl6::Grammar.O(':prec<y=>, :assoc<unary>', '%methodcall');
    Perl6::Grammar.O(':prec<x=>, :assoc<unary>', '%autoincrement');
    Perl6::Grammar.O(':prec<w=>, :assoc<right>', '%exponentiation');
    Perl6::Grammar.O(':prec<v=>, :assoc<unary>', '%symbolic_unary');
    Perl6::Grammar.O(':prec<u=>, :assoc<left>',  '%multiplicative');
    Perl6::Grammar.O(':prec<t=>, :assoc<left>',  '%additive');
    Perl6::Grammar.O(':prec<s=>, :assoc<left>',  '%replication');
    Perl6::Grammar.O(':prec<r=>, :assoc<left>',  '%concatenation');
    Perl6::Grammar.O(':prec<q=>, :assoc<list>', '%junctive_and');
    Perl6::Grammar.O(':prec<p=>, :assoc<list>', '%junctive_or');
    Perl6::Grammar.O(':prec<o=>, :assoc<unary>', '%named_unary');
    Perl6::Grammar.O(':prec<n=>, :assoc<left>',  '%structural');
    Perl6::Grammar.O(':prec<m=>, :assoc<left>, :pasttype<chain>',  '%chaining');
    Perl6::Grammar.O(':prec<l=>, :assoc<left>',  '%tight_and');
    Perl6::Grammar.O(':prec<k=>, :assoc<list>',  '%tight_or');
    Perl6::Grammar.O(':prec<j=>, :assoc<right>', '%conditional');
    Perl6::Grammar.O(':prec<i=>, :assoc<right>', '%item_assignment');
    Perl6::Grammar.O(':prec<h=>, :assoc<unary>', '%loose_unary');
    Perl6::Grammar.O(':prec<g=>, :assoc<list>, :nextterm<nulltermish>',  '%comma');
    Perl6::Grammar.O(':prec<f=>, :assoc<list>',  '%list_infix');
    Perl6::Grammar.O(':prec<e=>, :assoc<right>', '%list_assignment');   # XXX
    Perl6::Grammar.O(':prec<e=>, :assoc<right>', '%list_prefix');
    Perl6::Grammar.O(':prec<d=>, :assoc<left>',  '%loose_and');
    Perl6::Grammar.O(':prec<c=>, :assoc<left>',  '%loose_or');
}


token infixish {
    | <OPER=infix> <![=]>
    | <infix> <OPER=infix_postfix_meta_operator>
}

token postfixish {
    # last whitespace didn't end here
    <!MARKED('ws')>

    [ <.unsp> | '\\' ]?

    <postfix_prefix_meta_operator>?
    [
    | <OPER=dotty>
    | <OPER=privop>
    | <OPER=postfix>
    | <OPER=postcircumfix>
    ]
}

token postop {
    | <postfix>
    | <postcircumfix>
}

proto token infix_postfix_meta_operator { <...> }

proto token postfix_prefix_meta_operator { <...> }

token postfix_prefix_meta_operator:sym< » > {
    [ <sym> | '>>' ] <!before '('>
}

proto token dotty { <...> }
token dotty:sym<.> {
    <sym> <dottyop>
    <O('%methodcall')>
}

token dotty:sym<.*> {
    $<sym>=['.' [ <[+*?=]> | '^' '!'? ]] <dottyop>
    <O('%methodcall')>
}

token dottyop {
    [
    | <methodop>
    | <!alpha> <postop>
    ]
}

token privop { 
    '!' <methodop>
    <O('%methodcall')>
}

token methodop {
    [
    | <identifier>
    | <?before <[ ' " ]> >
        <quote>
        [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments"> ]
    ] <.unsp>? 
    [ 
    | <?[(]> <args>
    | ':' \s <args=.arglist>
    ]?
}

token dottyopish {
    <term=.dottyop>
}

token postcircumfix:sym<[ ]> { 
    '[' <.ws> <EXPR> ']' 
    <O('%methodcall')>
}

token postcircumfix:sym<{ }> {
    '{' <.ws> <EXPR> '}'
    <O('%methodcall')>
}

token postcircumfix:sym<ang> {
    <?[<]> <quote_EXPR: ':q'>
    <O('%methodcall')>
}

token postcircumfix:sym<( )> { 
    '(' <.ws> <arglist> ')' 
    <O('%methodcall')>
}

token postfix:sym<i>  { <sym> >> <O('%methodcall')> }

token prefix:sym<++>  { <sym>  <O('%autoincrement')> }
token prefix:sym<-->  { <sym>  <O('%autoincrement')> }
token postfix:sym<++> { <sym>  <O('%autoincrement')> }
token postfix:sym<--> { <sym>  <O('%autoincrement')> }

token infix:sym<**>   { <sym>  <O('%exponentiation')> }

token prefix:sym<+>   { <sym>  <O('%symbolic_unary, :pirop<set N*>')> }
token prefix:sym<~>   { <sym>  <O('%symbolic_unary')> }
token prefix:sym<->   { <sym> <![>]> <O('%symbolic_unary')> }
token prefix:sym<?>   { <!before '???'> <sym>  <O('%symbolic_unary')> }
token prefix:sym<!>   { <!before '!!!'> <sym>  <O('%symbolic_unary')> }
token prefix:sym<+^>  { <sym>  <O('%symbolic_unary')> }
token prefix:sym<^>   { <sym>  <O('%symbolic_unary')> }
token prefix:sym<|>   { <sym>  <O('%symbolic_unary')> }

token infix:sym<*>    { <sym>  <O('%multiplicative')> }
token infix:sym</>    { <sym>  <O('%multiplicative')> }
token infix:sym<div>  { <sym>  <O('%multiplicative')> }
token infix:sym<%>    { <sym>  <O('%multiplicative')> }
token infix:sym<+&>   { <sym>  <O('%multiplicative')> }
token infix:sym<~&>   { <sym>  <O('%multiplicative')> }
token infix:sym<?&>   { <sym>  <O('%multiplicative')> }

token infix:sym<+>    { <sym>  <O('%additive')> }
token infix:sym<->    { <sym> <![>]> <O('%additive')> }
token infix:sym<+|>   { <sym>  <O('%additive')> }
token infix:sym<+^>   { <sym>  <O('%additive')> }
token infix:sym<~|>   { <sym>  <O('%additive')> }
token infix:sym<~^>   { <sym>  <O('%additive')> }
token infix:sym«+<»   { <sym>  <O('%additive')> }
token infix:sym«+>»   { <sym>  <O('%additive')> }
token infix:sym<?|>   { <sym>  <O('%additive')> }
token infix:sym<?^>   { <sym>  <O('%additive')> }

token infix:sym<x>    { <sym>  <O('%replication')> }
token infix:sym<xx>    { <sym>  <O('%replication')> }

token infix:sym<~>    { <sym>  <O('%concatenation')> }

token infix:sym<&>    { <sym> <O('%junctive_and')> }
token infix:sym<|>    { <sym> <O('%junctive_or')> }
token infix:sym<^>    { <sym> <O('%junctive_or')> }

token prefix:sym<abs>     { <sym> » <O('%named_unary')> }
token prefix:sym<defined> { <sym> » <O('%named_unary')> }

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
token infix:sym<===>  { <sym>  <O('%chaining')> }
token infix:sym<eqv>  { <sym>  <O('%chaining')> }
token infix:sym<before>  { <sym>  <O('%chaining')> }
token infix:sym<after>  { <sym>  <O('%chaining')> }
token infix:sym<~~>   { <sym>  <O('%chaining')> }
token infix:sym<!~~>  { <sym>  <O('%chaining')> }

token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

token infix:sym<||>   { <sym>  <O('%tight_or, :pasttype<unless>')> }
token infix:sym<^^>   { <sym>  <O('%tight_or, :pasttype<xor>')> }
token infix:sym<//>   { <sym>  <O('%tight_or, :pasttype<def_or>')> }
token infix:sym<min>  { <sym>  <O('%tight_or')> }
token infix:sym<max>  { <sym>  <O('%tight_or')> }

token infix:sym<?? !!> { 
    '??'
    <.ws>
    <EXPR('i=')>
    '!!'
    <O('%conditional, :reducecheck<ternary>, :pasttype<if>')> 
}

token infix:sym<:=> {
    <sym>  <O('%item_assignment, :reducecheck<bindish_check>')>
}

method bindish_check($/) {
    # Do we have a sigature on the LHS? If so, use that rather
    # than the list.
    if pir::defined__IP($/[0].ast()<signature_from_declarator>) {
        $/[0] := $/[0].ast()<signature_from_declarator>;
        $/[0].bind_target('lexical');
    }
}

token infix:sym<::=> {
    <sym>  <O('%item_assignment')> 
    <.panic: "::= binding not yet implemented">
}

token infix:sym<.=> { <sym> <O('%item_assignment, :nextterm<dottyopish>')> }

token infix_postfix_meta_operator:sym<=> { '=' <O('%item_assignment')> }

token infix:sym«=>» { <sym> <O('%item_assignment')> }

token prefix:sym<so> { <sym> >> <O('%loose_unary')> }
token prefix:sym<not>  { <sym> >> <O('%loose_unary')> }

token infix:sym<,>    { <sym>  <O('%comma')> }

token infix:sym<Z>    { <sym>  <O('%list_infix')> }

token infix:sym<...>  { <sym>  <O('%list_infix')> }
# token term:sym<...>   { <sym> <args>? <O(|%list_prefix)> }

token infix:sym<=>    { <sym>  <O('%list_assignment, :reducecheck<assign_check>')> }

method assign_check($/) {
    my $lhs_ast := $/[0].ast;
    my $rhs_ast := $/[1].ast;
    if $lhs_ast<attribute_data> {
        $lhs_ast<attribute_data><build> := Perl6::Actions::make_attr_init_closure($rhs_ast);
        $/<drop> := 1;
    }
}

token infix:sym<and>  { <sym>  <O('%loose_and, :pasttype<if>')> }

token infix:sym<or>   { <sym>  <O('%loose_or, :pasttype<unless>')> }
token infix:sym<xor>  { <sym>  <O('%loose_or, :pasttype<xor>')> }
token infix:sym<err>  { <sym>  <O('%loose_or, :pasttype<def_or>')> }

token infix:sym<..>   { <sym> <O('%structural')> }
token infix:sym<^..>  { <sym> <O('%structural')> }
token infix:sym<..^>  { <sym> <O('%structural')> }
token infix:sym<^..^> { <sym> <O('%structural')> }

token infix:sym<leg>  { <sym> <O('%structural')> }
token infix:sym<cmp>  { <sym> <O('%structural')> }
token infix:sym«<=>»  { <sym> <O('%structural')> }

token infix:sym<but>  { <sym> <O('%structural')> }
token infix:sym<does> { <sym> <O('%structural')> }

grammar Perl6::Regex is Regex::P6Regex::Grammar {
    token metachar:sym<:my> { 
        ':' <?before 'my'> <statement=.LANG('MAIN', 'statement')> <.ws> ';' 
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token codeblock {
        <block=.LANG('MAIN','block')>
    }
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
        $I0 = index '$@%&', sigil
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

