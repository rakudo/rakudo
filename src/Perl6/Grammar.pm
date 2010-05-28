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
    self.comp_unit;
}

method add_my_name($name, $up_levels = 0) {
    my @BLOCK := Q:PIR{ %r = get_hll_global ['Perl6';'Actions'], '@BLOCK' };

    # We need to flag up most re-declaration collisions.
    my $cur_decl := @BLOCK[$up_levels].symbol($name);
    if $cur_decl {
        if $*PKGDECL eq 'role' || $cur_decl<stub> {
            return 1;
        }
        else {
            pir::die("Illegal redeclaration of symbol '$name'");
        }
    }

    # Add it.
    @BLOCK[$up_levels].symbol($name, :does_abstraction(1));
    return 1;
}

method add_our_name($name, $up_levels = 0) {
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
    return self.add_my_name($name, $up_levels);
}

method add_name($name, $up_levels = 0) {
    if $*SCOPE eq 'augment' || $*SCOPE eq 'supersede' {
        unless self.is_name($name) {
            pir::die("Can't $*SCOPE $*PKGDECL that doesn't exist");
        }
        unless $*MONKEY_TYPING {
            pir::die("Can't $*SCOPE $*PKGDECL $name without 'use MONKEY_TYPING'");
        }
    }
    else {
        if $*SCOPE eq 'our' {
            self.add_our_name($name, $up_levels);
        }
        else {
            self.add_my_name($name, $up_levels);
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
    
    # Is it a package we're compiling?
    if %COMPILINGPACKAGES{$name} {
        return 1;
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
    :my $*QSIGIL := '';
    '::'
    [
    ||  <?before '(' | <alpha> >
        [
        | <identifier>
        | '(' ~ ')' <EXPR>
            <.panic: "Indirect name lookups not yet implemented">
        ]
    || <?before '::'> <.panic: "Name component may not be null">
    ]?
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
token spacey { <?before <[ \s \# ]> > }

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
    :my $*MONKEY_TYPING := 0;                  # whether augment/supersede are allowed
    :my $*SETTING_MODE := 0;                   # are we compiling the SETTING
    :my $*LEFTSIGIL;                           # sigil of LHS for item vs list assignment
    :my $*SCOPE := '';                         # which scope declarator we're under
    :my $*MULTINESS := '';                     # which multi declarator we're under
    :my $*QSIGIL := '';                        # sigil of current interpolation
    :my $*TYPENAME := '';
    <.newpad>
    <.outerlex>
    <.finishpad>
    {*} #= open
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
    :my $*QSIGIL := '';
    <!before <[\])}]> | $ >
    [
    | <statement_control>
    | <EXPR> <.ws>
        [
        || <?MARKED('endstmt')>
        || <statement_mod_cond> <statement_mod_loop>?
        || <statement_mod_loop>
            {
                my $sp := $<EXPR><statement_prefix>;
                if $sp && $sp<sym> eq 'do' {
                    my $s := $<statement_mod_loop>[0]<sym>;
                    $/.CURSOR.obs("do..." ~ $s, "repeat..." ~ $s);
                }
            }
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

token statement_control:sym<foreach> {
    <sym> <.nofun> <.obs("'foreach'", "'for'")>
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

token vnum {
    \d+ | '*'
}

token version {
    'v' <?before \d+> <vnum> ** '.' '+'?
}

token statement_control:sym<need> {
    <sym> <.ws>
    [
    | <version>
    | <module_name>
    ] ** ','
}

token statement_control:sym<import> {
    <sym> <.ws>
    <module_name> [ <.spacey> <arglist> ]? <.ws>
}

token statement_control:sym<use> {
    <sym> <.ws>
    [
    | <version>
    | <module_name> [ <.spacey> <arglist> ]?
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
token statement_mod_loop:sym<given> { <sym> :s <smexpr=.EXPR> }

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
token term:sym<sigterm>            { <sigterm> }

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

token term:sym<new> {
    'new' \h+ <longname> \h* <!before ':'> <.obs("C++ constructor syntax", "method call syntax")>
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

proto token special_variable { <...> }

token special_variable:sym<$!{ }> {
    '$!{' .*? '}'
    <.obs('${ ... } or %! variable', 'smart match against $!')>
}

token special_variable:sym<$~> {
    <sym> <?before \s | ',' | '=' <terminator> >
    <.obs('$~ variable', 'Form module')>
}

token special_variable:sym<$`> {
    <sym>  <?before \s | ',' | <terminator> >
    <.obs('$` variable', 'explicit pattern before <(')>
}

token special_variable:sym<$@> {
    <sym> <?before \W>
    <.obs('$@ variable as eval error', '$!')>
}

# TODO: use actual variable in error message
token special_variable:sym<$#> {
    <sym>
    [
    || \w+ <.obs('$#variable', '@variable.end')>
    || <.obs('$# variable', '.fmt')>
    ]
}

token special_variable:sym<$$> {
    <sym> <!alpha> <?before \s | ',' | <terminator> >
    <.obs('$$ variable', '$*PID')>
}
token special_variable:sym<$%> {
    <sym> <!before \w> <!sigil>
    <.obs('$% variable', 'Form module')>
}

# TODO: $^X and other "caret" variables

token special_variable:sym<$^> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$^ variable', 'Form module')>
}

token special_variable:sym<$&> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$& variable', '$/ or $()')>
}

token special_variable:sym<$*> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$* variable', '^^ and $$')>
}

token special_variable:sym<$)> {
    <sym> <?{ $*GOAL ne ')' }> <?before \s | ',' | <terminator> >
    <.obs('$) variable', '$*EGID')>
}

token special_variable:sym<$-> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$- variable', 'Form module')>
}

token special_variable:sym<$=> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$= variable', 'Form module')>
}

token special_variable:sym<@+> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<%+> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('%+ variable', '.to method')>
}

token special_variable:sym<$+[ ]> {
    '$+['
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<@+[ ]> {
    '@+['
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<@+{ }> {
    '@+{'
    <.obs('%+ variable', '.to method')>
}

token special_variable:sym<@-> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<%-> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('%- variable', '.from method')>
}

token special_variable:sym<$-[ ]> {
    '$-['
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<@-[ ]> {
    '@-['
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<%-{ }> {
    '@-{'
    <.obs('%- variable', '.from method')>
}

token special_variable:sym<$+> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$+ variable', 'Form module')>
}

token special_variable:sym<$[> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$[ variable', 'user-defined array indices')>
}

token special_variable:sym<$]> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$] variable', '$*PERL_VERSION')>
}

token special_variable:sym<$\\> {
    '$\\' <?before \s | ',' | '=' | <terminator> >
    <.obs('$\\ variable', "the filehandle's :ors attribute")>
}

token special_variable:sym<$|> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$| variable', ':autoflush on open')>
}

token special_variable:sym<$:> {
    <sym> <?before <[\x20\t\n\],=)}]> >
    <.obs('$: variable', 'Form module')>
}

token special_variable:sym<$;> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <.obs('$; variable', 'real multidimensional hashes')>
}

token special_variable:sym<$'> { #'
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
}

# TODO: $"

token special_variable:sym<$,> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$, variable', ".join() method")>
}

token special_variable:sym['$<'] {
    <sym> <!before \s* \w+ \s* '>' >
    <.obs('$< variable', '$*UID')>
}

token special_variable:sym«\$>» {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$> variable', '$*EUID')>
}

token special_variable:sym<$.> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$. variable', "the filehandle's .line method")>
}

token special_variable:sym<$?> {
    <sym> <?before \s | ',' | <terminator> >
    <.obs('$? variable as child error', '$!')>
}


token desigilname {
    [
#    | <?before '$' > <variable>
    | <longname>
    ]
}

token variable {
    <?before <sigil> {
        unless $*LEFTSIGIL {
            $*LEFTSIGIL := $<sigil>.Str;
        }
    }> {}
    [
    || '&'
        [
        | '[' ~ ']' <infixish>
        ]
    ||  [
        | <sigil> <twigil>? <desigilname>
        | <special_variable>
        | <sigil> $<index>=[\d+]
        | <sigil> <?[<[]> <postcircumfix>
        | $<sigil>=['$'] $<desigilname>=[<[/_!]>]
        ]
    ]
    [ <?{ $<twigil> && $<twigil>[0] eq '.' }>
        [ <.unsp> | '\\' | <?> ] <?before '('> <arglist=.postcircumfix>
    ]?
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
    <sym> :my $*PKGDECL := 'package';
    <package_def>
}
token package_declarator:sym<module> {
    <sym> :my $*PKGDECL := 'module';
    <package_def>
}
token package_declarator:sym<class> {
    <sym> :my $*PKGDECL := 'class';
    <package_def>
}
token package_declarator:sym<grammar> {
    <sym> :my $*PKGDECL := 'grammar';
    <package_def>
}
token package_declarator:sym<role> {
    <sym> :my $*PKGDECL := 'role';
    <package_def>
}

token package_declarator:sym<does> {
    <sym> <.ws>
    <typename>
}

rule package_def {
    :my $*IN_DECL := 'package';
    <.newpad>
    <def_module_name>?
    <trait>*
    {*} #= open
    [
    || ';'
        <.finishpad>
        <statementlist>
    || <?[{]> <blockoid>
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
    | <type_declarator>
    ]
}

proto token multi_declarator { <...> }
token multi_declarator:sym<multi> {
    <sym> :my $*MULTINESS := 'multi';
    <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed multi'> ]
}
token multi_declarator:sym<proto> {
    <sym> :my $*MULTINESS := 'proto';
    <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed proto'> ]
}
token multi_declarator:sym<only> {
    <sym> :my $*MULTINESS := 'only';
    <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed only'> ]
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
token scope_declarator:sym<anon>      { <sym> <scoped('anon')> }
token scope_declarator:sym<supersede> {
    <sym> <.panic: '"supersede" not yet implemented'>
}

rule scoped($*SCOPE) {
    :my $*TYPENAME := '';
    [
    | <DECL=variable_declarator>
    | <DECL=routine_declarator>
    | <DECL=package_declarator>
    | <DECL=type_declarator>
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
    {
        if $<deflongname> && $<deflongname>[0]<colonpair> {
            $/.CURSOR.gen_op_if_needed($<deflongname>[0]);
        }
    }
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

token sigterm {
    ':(' ~ ')' <fakesignature>
}

token fakesignature {
    <.newpad>
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
    { $*LEFTSIGIL := '@'; }
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

    # enforce zone constraints
    {
        my $kind :=
            $<named_param>                     ?? '*' !! 
            $<quant> eq '?'                    ?? '?' !!
            $<quant> eq '!'                    ?? '!' !!
            $<quant> ne '' && $<quant> ne '\\' ?? '*' !!
                                                  '!';

        if $kind eq '!' {
            if $*zone eq 'posopt' {
                $/.CURSOR.panic("Can't put required parameter after optional parameters");
            }
            elsif $*zone eq 'var' {
                $/.CURSOR.panic("Can't put required parameter after variadic parameters");
            }
        }
        elsif $kind eq '?' {
            if $*zone  eq 'posreq' {
                    $*zone := 'posopt';
            }
            elsif $*zone eq  'var' {
                $/.CURSOR.panic("Can't put optional positional parameter after variadic parameters");
            }
        }
        elsif $kind eq '*' {
            $*zone := 'var';
        }
    }
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

proto token regex_declarator { <...> }
token regex_declarator:sym<rule> {
    <sym> {*} #= open
    :my $*METHODTYPE := 'rule';
    <regex_def>
}
token regex_declarator:sym<token> {
    <sym> {*} #= open
    :my $*METHODTYPE := 'token';
    <regex_def>
}
token regex_declarator:sym<regex> {
    <sym> {*} #= open
    :my $*METHODTYPE := 'regex';
    <regex_def>
}

rule regex_def {
    [
      <deflongname>?
      <.newpad>
      [ [ ':'?'(' <signature> ')'] | <trait> ]*
      {*} #= open
      '{'[ '<...>' |<p6regex=.LANG('Regex','nibbler')>]'}'<?ENDSTMT>
    ] || <.panic: "Malformed regex">
}

proto token type_declarator { <...> }

token type_declarator:sym<enum> {
    <sym> <.ws>
    <name>? <.ws>
    <?before '(' | '<' | '<<' | '«' > <circumfix>
}

token type_declarator:sym<subset> {
    <sym> :my $*IN_DECL := 'subset';
    :s
    [
        [
            [ <longname> { $/.CURSOR.add_name($<longname>[0].Str); } ]?
            <trait>*
            [ where <EXPR('e=')> ]?
        ]
        || <.panic: 'Malformed subset'>
    ]
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
token trait_mod:sym<hides>   {
    <sym>:s <module_name>
    [
    || <?{ $/.CURSOR.is_name($<module_name><longname>.Str) }>
    || <panic("Typename " ~ $<module_name> ~ " must be pre-declared to use it with hides")>
    ]
}
token trait_mod:sym<does>    {
    <sym>:s <module_name>
    [
    || <?{ $/.CURSOR.is_name($<module_name><longname>.Str) }>
    || <panic("Typename " ~ $<module_name> ~ " must be pre-declared to use it with does")>
    ]
}
token trait_mod:sym<will>    { <sym>:s <identifier> <pblock> }
token trait_mod:sym<of>      { <sym>:s <typename> }
token trait_mod:sym<as>      { <sym>:s <typename> }
token trait_mod:sym<returns> { <sym>:s <typename> }
token trait_mod:sym<handles> { <sym>:s <term> }


## Terms

proto token term { <...> }

token term:sym<YOU_ARE_HERE> { <sym> <.nofun> }

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
    :my $*QSIGIL := '';
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
token number:sym<complex>  { <im=.numish>'\\'?'i' }
token number:sym<numish>   { <numish> }

token numish {
    [
    | <dec_number>
    | <integer>
    | <rad_number>
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

token rad_number {
     <!before '::'> ':' $<radix> = [\d+] <.unsp>?
    # {}           # don't recurse in lexer
    # :dba('number in radix notation')
    [
    || '<'
            $<intpart> = [ <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]
            $<fracpart> = [ '.' <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]?
            [ '*' <base=.radint> '**' <exp=.radint> ]?
       '>'
    # || <?before '['> <circumfix>
    || <?before '('> <circumfix>
    || <.panic: "Malformed radix number">
    ]
}

token radint {
    [
    | <integer>
    # | <?before ':'\d> <rad_number> <?{
    #                         defined $<rad_number><intpart>
    #                         and
    #                         not defined $<rad_number><fracpart>
    #                     }>
    ]
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
token quote:sym<apos>  { <?[']>                <quote_EXPR: ':q'>  }
token quote:sym<dblq>  { <?["]>                <quote_EXPR: ':qq'> }
token quote:sym<q>     { 'q'   >> <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qq>    { 'qq'  >> <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<qx>    { 'qx'  >> <![(]> <.ws> <quote_EXPR: ':q'>  }
token quote:sym<qqx>   { 'qqx' >> <![(]> <.ws> <quote_EXPR: ':qq'> }
token quote:sym<Q>     { 'Q'   >> <![(]> <.ws> <quote_EXPR> }
token quote:sym<Q:PIR> { 'Q:PIR'      <.ws> <quote_EXPR> }
token quote:sym</null/> { '/' \s* '/' <.panic: "Null regex not allowed"> }
token quote:sym</ />  { '/'<p6regex=.LANG('Regex','nibbler')>'/' <.old_rx_mods>? }
token quote:sym<rx>   {
    <sym> >> 
    [
    | '/'<p6regex=.LANG('Regex','nibbler')>'/' <.old_rx_mods>?
    | '{'<p6regex=.LANG('Regex','nibbler')>'}' <.old_rx_mods>?
    ]
}
token quote:sym<m> {
    <sym> >>
    [
    | '/'<p6regex=.LANG('Regex','nibbler')>'/' <.old_rx_mods>?
    | '{'<p6regex=.LANG('Regex','nibbler')>'}'
    ]
}
token quote:sym<s> {
    <sym> >>
    [
    | '/' <p6regex=.LANG('Regex','nibbler')> <?[/]> <quote_EXPR: ':qq'> <.old_rx_mods>?
    | '[' <p6regex=.LANG('Regex','nibbler')> ']'
      <.ws> [ '=' || <.panic: "Missing assignment operator"> ]
      <.ws> <EXPR('i')>
    ]
}

token old_rx_mods {
    (<[ i g s m x c e ]>)
    {
        my $m := $/[0].Str;
        if    $m eq 'i' { $/.CURSOR.obs('/i',':i');                                   }
        elsif $m eq 'g' { $/.CURSOR.obs('/g',':g');                                   }
        elsif $m eq 's' { $/.CURSOR.obs('/s','^^ and $$ anchors');                    }
        elsif $m eq 'm' { $/.CURSOR.obs('/m','. or \N');                              }
        elsif $m eq 'x' { $/.CURSOR.obs('/x','normal default whitespace');            }
        elsif $m eq 'c' { $/.CURSOR.obs('/c',':c or :p');                             }
        elsif $m eq 'e' { $/.CURSOR.obs('/e','interpolated {...} or s{} = ... form'); }
        else            { $/.CURSOR.obs('suffix regex modifiers','prefix adverbs');   }
    }
}

token quote_escape:sym<$> {
    <?[$]>
    :my $*QSIGIL := '$';
    <?quotemod_check('s')> <EXPR('y=')>
}

token quote_escape:sym<array> {
    <?[@]>
    :my $*QSIGIL := '@';
    <?quotemod_check('a')> <EXPR('y=')>
}

token quote_escape:sym<%> {
    <?[%]>
    :my $*QSIGIL := '%';
    <?quotemod_check('h')> <EXPR('y=')>
}

token quote_escape:sym<&> {
    <?[&]>
    :my $*QSIGIL := '&';
    <?quotemod_check('f')> <EXPR('y=')>
}

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
token circumfix:sym<sigil> {
    <sigil> '(' ~ ')' <semilist>
    { unless $*LEFTSIGIL { $*LEFTSIGIL := $<sigil>.Str } }
}

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
    Perl6::Grammar.O(':prec<b=>, :assoc<list>',  '%sequencer');
}

token termish {
    :my $*SCOPE := "";
    <prefixish>*
    <term>
    [
    || <?{ $*QSIGIL }>
        [
        || <?{ $*QSIGIL eq '$' }> [ <postfixish>+! <?{ bracket_ending($<postfixish>) }> ]?
        ||                          <postfixish>+! <?{ bracket_ending($<postfixish>) }>
        ]
    || <!{ $*QSIGIL }> <postfixish>*
    ]
}

sub bracket_ending($matches) {
    my $check := $matches[+$matches - 1];
    my $str   := $check.Str;
    my $last  := pir::substr($str, pir::length__IS($check) - 1, 1);
    $last eq ')' || $last eq '}' || $last eq ']' || $last eq '>'
}

method EXPR($preclim = '') {
    # Override this so we can set $*LEFTSIGIL.
    my $*LEFTSIGIL := '';
    pir::find_method__pps(HLL::Grammar, 'EXPR')(self, $preclim);
}

token prefixish { 
    [
    | <OPER=prefix>
    | <OPER=prefix_circumfix_meta_operator>
    ]
    <prefix_postfix_meta_operator>?
    <.ws>
}

token infixish {
    [
    | '[' ~ ']' <infixish> <OPER=.copyOPER('infixish')>
    | <OPER=infix_circumfix_meta_operator>
    | <OPER=infix> <![=]>
    | <OPER=infix_prefix_meta_operator>
    | <infix> <OPER=infix_postfix_meta_operator>
    ]
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
    { $*LEFTSIGIL := '@'; }
}

token postop {
    | <postfix>
    | <postcircumfix>
}

proto token prefix_circumfix_meta_operator { <...> }

proto token infix_postfix_meta_operator { <...> }

proto token infix_prefix_meta_operator { <...> }

proto token infix_circumfix_meta_operator { <...> }

proto token postfix_prefix_meta_operator { <...> }

proto token prefix_postfix_meta_operator { <...> }

regex prefix_circumfix_meta_operator:sym<reduce> {
    :my $*IN_REDUCE := 1;
    <?before '['\S+']'>

    '['
    [
    || <op=.infixish> <?before ']'>
    || $<triangle>=[\\]<op=.infixish> <?before ']'>
    || <!>
    ]
    ']'

    <O('%list_prefix, :assoc<unary>, :uassoc<left>')>
}

token postfix_prefix_meta_operator:sym<»> {
    [ <sym> | '>>' ] 
    [ <!{ $*QSIGIL }> || <!before '('> ]
}

token prefix_postfix_meta_operator:sym<«> {
    <sym> | '<<'
}

token infix_circumfix_meta_operator:sym<« »> {
    $<opening>=[
    | '«'
    | '»'
    ]
    {} <infixish>
    $<closing>=[ '«' | '»' || <.panic("Missing « or »")> ]
    <O=.copyO('infixish')>
}

token infix_circumfix_meta_operator:sym«<< >>» {
    $<opening>=[
    | '<<'
    | '>>'
    ]
    {} <infixish>
    $<closing>=[ '<<' | '>>' || <.panic("Missing << or >>")> ]
    <O=.copyO('infixish')>
}

method copyO($from) {
    # There must be a a better way, but until pmichaud++ shows us it,
    # this is the best I can come up with. :-) -- jnthn
    my $m := self.MATCH();
    my $r := $m{$from}<OPER><O>;
    Q:PIR {
        (%r, $I0) = self.'!cursor_start'()
        %r.'!cursor_pass'($I0, '')
        $P0 = find_lex '$r'
        setattribute %r, '$!match', $P0
    };
}

method copyOPER($from) {
    # There must be a a better way, but until pmichaud++ shows us it,
    # this is the best I can come up with. :-) -- jnthn
    my $m := self.MATCH();
    my $r := $m{$from}<OPER>;
    Q:PIR {
        (%r, $I0) = self.'!cursor_start'()
        %r.'!cursor_pass'($I0, '')
        $P0 = find_lex '$r'
        setattribute %r, '$!match', $P0
    };
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
    | <longname>
    | <?before '$' | '@' | '&' > <variable>
    | <?before <[ ' " ]> >
        [ <!{$*QSIGIL}> || <!before '"' <-["]>*? \s > ] # dwim on "$foo."
        <quote>
        [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments"> ]
    ] <.unsp>?
    [
    | <?[(]> <args>
    | ':' \s <!{ $*QSIGIL }> <args=.arglist>
    ]?
}

token dottyopish {
    <term=.dottyop>
}

token postcircumfix:sym<[ ]> {
    :my $*QSIGIL := '';
    '[' ~ ']' [ <.ws> <semilist> ]
    <O('%methodcall')>
}

token postcircumfix:sym<{ }> {
    :my $*QSIGIL := '';
    '{' ~ '}' [ <.ws> <semilist> ]
    <O('%methodcall')>
}

token postcircumfix:sym<ang> {
    <?[<]> <quote_EXPR: ':q', ':w'>
    <O('%methodcall')>
}

token postcircumfix:sym<( )> {
    '(' ~ ')' [ <.ws> <arglist> ]
    <O('%methodcall')>
}

token postfix:sym<i>  { <sym> >> <O('%methodcall')> }

token prefix:sym<++>  { <sym>  <O('%autoincrement')> }
token prefix:sym<-->  { <sym>  <O('%autoincrement')> }
token postfix:sym<++> { <sym>  <O('%autoincrement')> }
token postfix:sym<--> { <sym>  <O('%autoincrement')> }

# TODO: report the correct bracket in error message
token postfix:sym«->» {
    <sym>
    [
    |  ['[' | '{' | '(' ] <.obs('->(), ->{} or ->[] as postfix dereferencer', '.(), .[] or .{} to deref, or whitespace to delimit a pointy block')>
    | <.obs('-> as postfix', 'either . to call a method, or whitespace to delimit a pointy block')>
    ]
}

token infix:sym<**>   { <sym>  <O('%exponentiation')> }

token prefix:sym<+>   { <sym>  <O('%symbolic_unary')> }
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

token infix:sym«<<» { <sym> \s <.obs('<< to do left shift', '+< or ~<')> }

token infix:sym«>>» { <sym> \s <.obs('>> to do right shift', '+> or ~>')> }


token infix:sym<+>    { <sym>  <O('%additive')> }
token infix:sym<->    { <sym> <![>]> <O('%additive')> }
token infix:sym<+|>   { <sym>  <O('%additive')> }
token infix:sym<+^>   { <sym>  <O('%additive')> }
token infix:sym<~|>   { <sym>  <O('%additive')> }
token infix:sym<~^>   { <sym>  <O('%additive')> }
token infix:sym«+<»   { <sym> <!before '<'> <O('%additive')> }
token infix:sym«+>»   { <sym> <!before '>'> <O('%additive')> }
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
token infix:sym«!=»   { <sym> <?before \s> <O('%chaining')> }
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

token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

token infix:sym<||>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<unless>')> }
token infix:sym<^^>   { <sym>  <O('%tight_or, :pasttype<xor>')> }
token infix:sym<//>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<def_or>')> }
token infix:sym<min>  { <sym>  <O('%tight_or')> }
token infix:sym<max>  { <sym>  <O('%tight_or')> }

token infix:sym<?? !!> {
    '??'
    <.ws>
    <EXPR('i=')>
    '!!'
    <O('%conditional, :reducecheck<ternary>, :pasttype<if>')>
}

token infix_prefix_meta_operator:sym<!> {
    <sym> <infixish> 
    [
    || <?{ $<infixish>.Str eq '=' }> <O('%chaining')>
    || <O=.copyO('infixish')>
    ]
}
token infix_prefix_meta_operator:sym<R> { <sym> <infixish> <O=.copyO('infixish')> }
token infix_prefix_meta_operator:sym<S> { <sym> <infixish> <O=.copyO('infixish')> }
token infix_prefix_meta_operator:sym<X> { <sym> <infixish> <O('%list_infix')> }
token infix_prefix_meta_operator:sym<Z> { <sym> <infixish> <O('%list_infix')> }
token infix:sym<minmax> { <sym>  <O('%list_infix')> }

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

# Should probably have <!after '='> to agree w/spec, but after NYI.
# Modified infix != below instead to prevent misparse
token infix_postfix_meta_operator:sym<=> { '=' <O('%item_assignment')> }

token infix:sym«=>» { <sym> <O('%item_assignment')> }

token prefix:sym<so> { <sym> >> <O('%loose_unary')> }
token prefix:sym<not>  { <sym> >> <O('%loose_unary')> }

token infix:sym<,>    { <sym>  <O('%comma')> }

token infix:sym<Z>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }
token infix:sym<X>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }

token infix:sym<...>  { <sym>  <O('%list_infix')> }
# token term:sym<...>   { <sym> <args>? <O(|%list_prefix)> }

token infix:sym<=> {
    <sym>
    [
    || <?{ $*LEFTSIGIL eq '$' }> <O('%item_assignment, :reducecheck<assign_check>')>
    || <O('%list_assignment, :reducecheck<assign_check>')>
    ]
}

method assign_check($/) {
    my $lhs_ast := $/[0].ast;
    my $rhs_ast := $/[1].ast;
    if $lhs_ast && $lhs_ast<attribute_data> {
        $lhs_ast<attribute_data><build> := Perl6::Actions::make_attr_init_closure($rhs_ast);
        $/<drop> := 1;
    }
}

token infix:sym<and>  { <sym>  <O('%loose_and, :pasttype<if>')> }

token infix:sym<or>   { <sym>  <O('%loose_or, :pasttype<unless>')> }
token infix:sym<xor>  { <sym>  <O('%loose_or, :pasttype<xor>')> }
token infix:sym<err>  { <sym>  <O('%loose_or, :pasttype<def_or>')> }

token infix:sym«<==»  { <sym> <O('%sequencer')> }
token infix:sym«==>»  { <sym> <O('%sequencer')> }
token infix:sym«<<==» { <sym> <O('%sequencer')> }
token infix:sym«==>>» { <sym> <O('%sequencer')> }

token infix:sym<..>   { <sym> <O('%structural')> }
token infix:sym<^..>  { <sym> <O('%structural')> }
token infix:sym<..^>  { <sym> <O('%structural')> }
token infix:sym<^..^> { <sym> <O('%structural')> }

token infix:sym<leg>  { <sym> <O('%structural')> }
token infix:sym<cmp>  { <sym> <O('%structural')> }
token infix:sym«<=>»  { <sym> <O('%structural')> }

token infix:sym<but>  { <sym> <O('%structural')> }
token infix:sym<does> { <sym> <O('%structural')> }

token infix:sym<!~> { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> <O('%chaining')> }
token infix:sym<=~> { <sym> <.obs('=~ to do pattern matching', '~~')> <O('%chaining')> }

grammar Perl6::Regex is Regex::P6Regex::Grammar {
    token metachar:sym<:my> {
        ':' <?before 'my'> <statement=.LANG('MAIN', 'statement')> <.ws> ';'
    }

    token metachar:sym<{ }> {
        <?[{]> <codeblock>
    }

    token metachar:sym<rakvar> {
        <?[$@&]> <?before .<?alpha>> <var=.LANG('MAIN', 'variable')>
    }

    token assertion:sym<{ }> {
        <?[{]> <codeblock>
    }

    token assertion:sym<?{ }> {
        $<zw>=[ <[?!]> <?before '{'> ] <codeblock>
    }

    token assertion:sym<var> {
        <?[$@&]> <var=.LANG('MAIN', 'variable')>
    }

    token codeblock {
        <block=.LANG('MAIN','block')>
    }

    token assertion:sym<name> {
        $<longname>=[\w+]
            [
            | <?before '>'>
            | '=' <assertion>
            | ':' <arglist>
            | '(' <arglist=p6arglist> ')'
            | <.normspace> <nibbler>
            ]?
    }

    token p6arglist {
        <arglist=.LANG('MAIN','arglist')>
    }
}

our %is_sigil;
INIT {
    our %is_sigil;
    %is_sigil{'$'} := 1;
    %is_sigil{'@'} := 1;
    %is_sigil{'%'} := 1;
    %is_sigil{'&'} := 1;
}


sub parse_name($name) {
    my $type_param := '';
    my $sep := pir::index__ISS($name,'[');
    if ($sep > -1) {
        $type_param := pir::substr__SSII($name, $sep);
        $name := pir::substr__SSII($name, 0, $sep);
    }

    my @parts := pir::split__PSS('::', $name);
    my $sigil := pir::substr__SSII(@parts[0], 0, 1);
    if %is_sigil{$sigil} {
        @parts[0] := pir::substr__SSII(@parts[0], 1);
        my $last_part := @parts.pop();
        $last_part := $sigil ~ $last_part;
        @parts.push($last_part);
    }

    my @result;
    for @parts {
        @result.push($_) if $_;
    }

    if $type_param {
        my $last_part := @result.pop();
        $last_part := $last_part ~ $type_param;
        @result.push($last_part);
    }

    @result;
}


# This sub is used to augment the grammar with new ops at parse time.
method gen_op_if_needed($deflongname) {
    my $self := Q:PIR { %r = self };

    # Extract interesting bits from the longname.
    my $category := $deflongname<name>.Str;
    my $opname   := ~$deflongname<colonpair>[0]<circumfix><quote_EXPR><quote_delimited><quote_atom>[0];
    my $canname  := $category ~ ":sym<" ~ $opname ~ ">";

    # Work out what default precedence we want.
    my $prec;
    if $category eq 'infix' {
        $prec := '%additive';
    }
    elsif $category eq 'prefix' {
        $prec := '%symbolic_unary';
    }
    elsif $category eq 'postfix' {
        $prec := '%autoincrement';
    }
    else {
        return;
    }

    # Check if we have the op already.
    unless pir::can__IPS($self, $canname) {
        # Nope, so we need to modify the grammar. Build code to parse it.
        my $parse := Regex::P6Regex::Actions::buildsub(PAST::Regex.new(
            :pasttype('concat'),
            PAST::Regex.new(
                :pasttype('subcapture'),
                :name('sym'),
                :backtrack('r'),
                PAST::Regex.new(
                    :pasttype('literal'),
                    $opname
                )
            ),
            PAST::Regex.new(
                :pasttype('subrule'),
                :name('O'),
                :backtrack('r'),
                'O',
                PAST::Val.new( :value($prec) )
            )
        ));

        # Needs to go into the Perl6::Grammar namespace.
        $parse.name($canname);
        $parse.namespace(pir::split('::', 'Perl6::Grammar'));

        # Compile and then install the two produced methods into the
        # Perl6::Grammar methods table.
        my $compiled := PAST::Compiler.compile($parse);
        $self.HOW.add_method($self, ~$compiled[0], $compiled[0]);
        $self.HOW.add_method($self, ~$compiled[1], $compiled[1]);

        # Mark proto-regex table as needing re-generation.
        Q:PIR {
            $P0 = find_lex '$self'
            $P0.'!protoregex_generation'()
        };
    }
}
