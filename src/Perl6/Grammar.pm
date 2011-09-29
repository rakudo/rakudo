use NQPP6Regex;
use QRegex;
use Perl6::SymbolTable;
use Perl6::Pod;

grammar Perl6::Grammar is HLL::Grammar {
    method TOP() {
        # Language braid.
        my %*LANG;
        %*LANG<Regex>         := Perl6::RegexGrammar;
        %*LANG<Regex-actions> := Perl6::RegexActions;
        %*LANG<MAIN>          := Perl6::Grammar;
        %*LANG<MAIN-actions>  := Perl6::Actions;
        
        # Package declarator to meta-package mapping. Starts pretty much empty;
        # we get the mappings either imported or supplied by the setting. One
        # issue is that we may have no setting to provide them, e.g. when we
        # compile the setting, but it still wants some kinda package. We just
        # fudge in knowhow for that.
        my %*HOW;
        %*HOW<knowhow> := pir::get_knowhow__P();
        %*HOW<package> := pir::get_knowhow__P();
        
        # Symbol table and serialization context builder - keeps track of
        # objects that cross the compile-time/run-time boundary that are
        # associated with this compilation unit.
        my $file := pir::find_caller_lex__ps('$?FILES');
        my $*ST := pir::isnull($file) ??
            Perl6::SymbolTable.new(:handle(~pir::time__N())) !!
            Perl6::SymbolTable.new(:handle(~pir::time__N()), :description($file));
            
        self.comp_unit;
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

    token end_keyword {
        <!before <[ \( \\ ' \- ]> || \h* '=>'> »
    }
    token spacey { <?before [\s | '#']> }

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
            [
            | \s+
            | <.unv>
            ]*
            <?MARKER('ws')>
    }

    token unv {
        # :dba('horizontal whitespace')
        [
        | ^^ <?before \h* '=' [ \w | '\\'] > <.pod_content_toplevel>
        | \h* <comment>
        | \h+
        ]
    }

    proto token comment { <...> }

    token comment:sym<#> {
       '#' {} \N*
    }

    token comment:sym<#`(...)> {
        '#`' {}
        [ <quote_EXPR> || <.panic: "Opening bracket is required for #` comment"> ]
    }

    token comment:sym<#=(...)> {
        '#=' <!ws> $<attachment>=<quote_EXPR>
    }

    token comment:sym<#=> {
        '#=' \h+ $<attachment>=[\N*]
        { $*DECLARATOR_DOCS := ~$<attachment> }
    }

    token attach_docs {
        {
            $*DECLARATOR_DOCS := '';
            if $*DOC ne '' {
                my $cont  := Perl6::Pod::serialize_aos(
                    [$*DOC]
                )<compile_time_value>;
                my $block := $*ST.add_constant(
                    'Pod::Block::Declarator', 'type_new',
                    :nocache, :content($cont),
                );
                $*DOCEE := $block<compile_time_value>;
                $*POD_BLOCKS.push($*DOCEE);
            }
        }
        <?>
    }

    token pod_content_toplevel {
        <pod_block>
    }

    proto token pod_content { <...> }

    token pod_content:sym<block> {
        <pod_newline>*
        <pod_block>
        <pod_newline>*
    }

    # any number of paragraphs of text
    token pod_content:sym<text> {
        <pod_newline>*
        <pod_textcontent> ** <pod_newline>+
        <pod_newline>*
    }

    # not a block, just a directive
    token pod_content:sym<config> {
        <pod_newline>*
        ^^ \h* '=config' \h+ $<type>=\S+ [ [\n '=']? \h+ <colonpair> ]+
        <pod_newline>+
    }

    proto token pod_textcontent { <...> }

    # text not being code
    token pod_textcontent:sym<regular> {
        $<spaces>=[ \h* ]
         <?{ !$*ALLOW_CODE
             || ($<spaces>.to - $<spaces>.from) <= $*VMARGIN }>

        $<text> = [
            \h* <!before '=' \w> <pod_string> <pod_newline>
        ] +
    }

    token pod_textcontent:sym<code> {
        $<spaces>=[ \h* ]
        <?{ $*ALLOW_CODE
            && ($<spaces>.to - $<spaces>.from) > $*VMARGIN }>
        $<text> = [
            [<!before '=' \w> \N+] ** [<pod_newline> $<spaces>]
        ]
    }

    token pod_formatting_code {
        $<code>=<[A..Z]>
        '<' { $*POD_IN_FORMATTINGCODE := 1 }
        $<content>=[ <!before '>'> <pod_string_character> ]+
        '>' { $*POD_IN_FORMATTINGCODE := 0 }
    }

    token pod_string {
        <pod_string_character>+
    }

    token pod_string_character {
        <pod_formatting_code> || $<char>=[ \N || [
            <?{ $*POD_IN_FORMATTINGCODE == 1}> \n <!before \h* '=' \w>
            ]
        ]
    }

    proto token pod_block { <...> }

    token pod_block:sym<delimited> {
        ^^
        $<spaces> = [ \h* ]
        '=begin'
        [ <?before <pod_newline>>
          <.panic('=begin must be followed by an identifier; '
                   ~ '(did you mean "=begin pod"?)')>
        ]?
        \h+ <!before 'END'>
        {
            $*VMARGIN    := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_CODE := 1 }
            || <identifier>
        ]
        [ [\n '=']? \h+ <colonpair> ]*
        <pod_newline>+
        [
         <pod_content> *
         ^^ $<spaces> '=end' \h+ $<type> <pod_newline>
         ||  <.panic: '=begin without matching =end'>
        ]
    }

    token pod_block:sym<delimited_raw> {
        ^^
        $<spaces> = [ \h* ]
        '=begin' \h+ <!before 'END'>
                        $<type>=[ 'code' || 'comment' ]
                        [ [\n '=']? \h+ <colonpair> ]*
                        <pod_newline>+
        [
         $<pod_content> = [ .*? ]
         ^^ $<spaces> '=end' \h+ $<type> <pod_newline>
         ||  <.panic: '=begin without matching =end'>
        ]
    }

    token pod_block:sym<delimited_table> {
        ^^ \h* '=begin' \h+ 'table'
            [ [\n '=']? \h+ <colonpair> ]* <pod_newline>+
        [
         <table_row>*
         ^^ \h* '=end' \h+ 'table' <pod_newline>
         ||  <.panic: '=begin without matching =end'>
        ]
    }

    token table_row {
        \h* <!before '=' \w> \N* \n
    }

    token pod_block:sym<end> {
        ^^ \h*
        [
            || '=begin' \h+ 'END' <pod_newline>
            || '=for'   \h+ 'END' <pod_newline>
            || '=END' <pod_newline>
        ]
        .*
    }

    token pod_block:sym<paragraph> {
        ^^
        $<spaces> = [ \h* ]
        '=for' \h+ <!before 'END'>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_CODE := 1 }
            || <identifier>
        ]
        [ [\n '=']? \h+ <colonpair> ]*
        <pod_newline>
        $<pod_content> = <pod_textcontent>?
    }

    token pod_block:sym<paragraph_raw> {
        ^^ \h* '=for' \h+ <!before 'END'>
                          $<type>=[ 'code' || 'comment' ]
                          [ [\n '=']? \h+ <colonpair> ]*
                          <pod_newline>
        $<pod_content> = [ \h* <!before '=' \w> \N+ \n ]+
    }

    token pod_block:sym<paragraph_table> {
        ^^ \h* '=for' \h+ 'table'
            [ [\n '=']? \h+ <colonpair> ]* <pod_newline>
        [ <!before \h* \n> <table_row>]*
    }

    token pod_block:sym<abbreviated> {
        ^^
        $<spaces> = [ \h* ]
        '=' <!before begin || end || for || END || config>
        {
            $*VMARGIN := $<spaces>.to - $<spaces>.from;
        }
        :my $*ALLOW_CODE := 0;
        $<type> = [
            <pod_code_parent> { $*ALLOW_CODE := 1 }
            || <identifier>
        ]
        [ [\n '=']? \h+ <colonpair> ]*
        \s
        $<pod_content> = <pod_textcontent>?
    }

    token pod_block:sym<abbreviated_raw> {
        ^^ \h* '=' $<type>=[ 'code' || 'comment' ]
        [ [\n '=']? \h+ <colonpair> ]* \s
        $<pod_content> = [ \h* <!before '=' \w> \N+ \n ]*
    }

    token pod_block:sym<abbreviated_table> {
        ^^ \h* '=table' [ [\n '=']? \h+ <colonpair> ]* <pod_newline>
        [ <!before \h* \n> <table_row>]*
    }

    token pod_newline {
        \h* \n
    }

    token pod_code_parent {
        'pod' <!before \w> || 'item' \d* <!before \w>
        # TODO: Also Semantic blocks one day
    }

    token install_doc_phaser { <?> }

    ## Top-level rules

    token comp_unit {
        # From STD.pm.
        :my $*LEFTSIGIL;                           # sigil of LHS for item vs list assignment
        :my $*SCOPE := '';                         # which scope declarator we're under
        :my $*MULTINESS := '';                     # which multi declarator we're under
        :my $*QSIGIL := '';                        # sigil of current interpolation
        :my $*IN_DECL;                             # what declaration we're in
        :my $*MONKEY_TYPING := 0;                  # whether augment/supersede are allowed
        :my $*begin_compunit := 1;                 # whether we're at start of a compilation unit
        :my $*DECLARAND;                           # the current thingy we're declaring, and subject of traits
        :my $*METHODTYPE;                          # the current type of method we're in, if any
        :my $*PKGDECL;                             # what type of package we're in, if any

        # Extras.
        :my %*METAOPGEN;                           # hash of generated metaops
        :my $*IMPLICIT;                            # whether we allow an implicit param
        :my $*FORBID_PIR := 0;                     # whether pir::op and Q:PIR { } are disallowed
        :my $*HAS_YOU_ARE_HERE := 0;               # whether {YOU_ARE_HERE} has shown up
        :my $*TYPENAME := '';
        :my $*VMARGIN    := 0;                     # pod stuff
        :my $*ALLOW_CODE := 0;                     # pod stuff
        :my $*POD_IN_FORMATTINGCODE := 0;          # pod stuff
        
        # Various interesting scopes we'd like to keep to hand.
        :my $*GLOBALish;
        :my $*PACKAGE;
        :my $*SETTING;
        :my $*UNIT;
        :my $*UNIT_OUTER;
        :my $*EXPORT;

        # A place for Pod
        :my $*POD_BLOCKS := [];
        :my $*POD_BLOCKS_SEEN := {};
        :my $*POD_PAST;
        :my $*DECLARATOR_DOCS;
        
        # CHECK phasers for this compilation unit we'll need at
        # CHECK time.
        :my @*CHECK_PHASERS := [];
        
        # Setting loading and symbol setup.
        {
            # Create unit outer (where we assemble any lexicals accumulated
            # from e.g. REPL) and the real UNIT.
            $*UNIT_OUTER := $*ST.push_lexpad($/);
            $*UNIT := $*ST.push_lexpad($/);
            $*UNIT<IN_DECL> := 'mainline';
            
            # If we already have a specified outer context, then that's
            # our setting. Otherwise, load one.
            unless pir::defined(%*COMPILING<%?OPTIONS><outer_ctx>) {
                $*SETTING := $*ST.load_setting(%*COMPILING<%?OPTIONS><setting> // 'CORE');
            }
            $/.CURSOR.unitstart();
            try {
                my $EXPORTHOW := $*ST.find_symbol(['EXPORTHOW']);
                for $EXPORTHOW.WHO {
                    %*HOW{$_.key} := $_.value;
                }
            }
            
            # Create GLOBAL(ish).
            $*GLOBALish := $*ST.pkg_create_mo(%*HOW<package>, :name('GLOBAL'));
            $*ST.pkg_compose($*GLOBALish);
                
            # Create EXPORT.
            $*EXPORT := $*ST.pkg_create_mo(%*HOW<package>, :name('EXPORT'));
            $*ST.pkg_compose($*EXPORT);
                
            # We start with the current package as that also.
            $*PACKAGE := $*GLOBALish;
            
            # Install unless we've no setting, in which case we've likely no
            # static lexpad class yet either. Also, UNIT needs a code object.
            unless %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
                $*ST.install_lexical_symbol($*UNIT, 'GLOBALish', $*GLOBALish);
                $*ST.install_lexical_symbol($*UNIT, 'EXPORT', $*EXPORT);
                $*ST.install_lexical_symbol($*UNIT, '$?PACKAGE', $*PACKAGE);
                $*ST.create_code_object($*UNIT, 'Block', $*ST.create_signature([]));
            }
        }
        
        <.finishpad>
        <statementlist>

        <.install_doc_phaser>
        
        [ $ || <.panic: 'Confused'> ]
        
        {
            $*POD_PAST := $*ST.add_constant(
                'Array', 'type_new', |$*POD_BLOCKS
            );
            $*ST.install_lexical_symbol(
                $*UNIT, '$=POD', $*POD_PAST<compile_time_value>
            );
            $*ST.pop_lexpad(); # UNIT
            $*ST.pop_lexpad(); # UNIT_OUTER
        }
        
        # CHECK time.
        { for @*CHECK_PHASERS { $_() } }
    }
    
    method import_EXPORTHOW($UNIT) {    
        # See if we've exported any HOWs.
        if pir::exists($UNIT, 'EXPORTHOW') {
            for $UNIT<EXPORTHOW>.WHO {
                %*HOW{$_.key} := pir::nqp_decontainerize__PP($_.value);
            }
        }
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
        :my $*SCOPE := '';
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
        :my $*CURPAD;
        <.finishpad>
        [
        | '{YOU_ARE_HERE}' <you_are_here>
        | '{' ~ '}' <statementlist> <?ENDSTMT>
        | <?terminator> <.panic: 'Missing block'>
        | <?> <.panic: 'Malformed block'>
        ]
        { $*CURPAD := $*ST.pop_lexpad() }
    }

    token unitstart { <?> }
    token you_are_here { <?> }
    token newpad { <?> { $*ST.push_lexpad($/) } }
    token finishpad { <?> }

    proto token terminator { <...> }

    token terminator:sym<;> { <?[;]> }
    token terminator:sym<)> { <?[)]> }
    token terminator:sym<]> { <?[\]]> }
    token terminator:sym<}> { <?[}]> }
    token terminator:sym<if>     { 'if'     <.end_keyword> }
    token terminator:sym<unless> { 'unless' <.end_keyword> }
    token terminator:sym<while>  { 'while'  <.end_keyword> }
    token terminator:sym<until>  { 'until'  <.end_keyword> }
    token terminator:sym<for>    { 'for'    <.end_keyword> }
    token terminator:sym<given>  { 'given'  <.end_keyword> }
    token terminator:sym<when>   { 'when'   <.end_keyword> }

    token stdstopper { <?terminator> }

    ## Statement control

    proto token statement_control { <...> }

    token statement_control:sym<if> {
        <sym> <.end_keyword> :s
        <xblock>
        [ 'elsif'\s <xblock> ]*
        [ 'else'\s <else=.pblock> ]?
    }

    token statement_control:sym<unless> {
        <sym> <.end_keyword> :s
        <xblock>
        [ <!before 'else'> || <.panic: 'unless does not take "else", please rewrite using "if"'> ]
    }

    token statement_control:sym<while> {
        $<sym>=[while|until] <.end_keyword> :s
        <xblock>
    }

    token statement_control:sym<repeat> {
        <sym> <.end_keyword> :s
        [
        | $<wu>=[while|until]\s <xblock>
        | <pblock> $<wu>=[while|until]\s <EXPR>
        ]
    }

    token statement_control:sym<for> {
        <sym> <.end_keyword> :s
        [ <?before 'my'? '$'\w+ '(' >
            <.panic: "This appears to be Perl 5 code"> ]?
        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
            <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
        <xblock(1)>
    }

    token statement_control:sym<foreach> {
        <sym> <.end_keyword> <.obs("'foreach'", "'for'")>
    }

    token statement_control:sym<loop> {
        <sym> <.end_keyword>
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
        {
            for $<module_name> {
                $*ST.load_module(~$_<longname>, $*GLOBALish);
            }
        }
    }

    token statement_control:sym<import> {
        <sym> <.ws>
        <module_name> [ <.spacey> <arglist> ]? <.ws>
        {
            my @name := parse_name(~$<module_name><longname>);
            my $module;
            my $found := 0;
            try { $module := $*ST.find_symbol(@name); $found := 1; }
            if $found {
                do_import($module.WHO, $<arglist>);
            }
            else {
                $/.CURSOR.panic("Could not find module " ~ ~$<module_name> ~
                    " to import symbols from");
            }
        }
    }

    token statement_control:sym<use> {
        :my $longname;
        :my $*IN_DECL := 'use';
        :my $*SCOPE   := 'use';
        $<doc>=[ 'DOC' \h+ ]?
        <sym> <.ws>
        [
        | <version>
        | <module_name>
            {
                $longname := $<module_name><longname>;
                
                # Some modules are handled in the actions are just turn on a
                # setting of some kind.
                if $longname.Str eq 'MONKEY_TYPING' {
                    $*MONKEY_TYPING := 1;
                    $longname := "";
                }
                elsif $longname.Str eq 'FORBID_PIR' ||
                      $longname.Str eq 'Devel::Trace' ||
                      $longname.Str eq 'fatal' {
                    $longname := "";
                }
            }
            [
            || <.spacey> <arglist>
                {
                    $/.CURSOR.panic("arglist case of use not yet implemented");
                }
            || { 
                    unless ~$<doc> && !%*COMPILING<%?OPTIONS><doc> {
                        if $longname {
                            my $module := $*ST.load_module(~$longname,
                                                           $*GLOBALish);
                            do_import($module, $<arglist>);
                            $/.CURSOR.import_EXPORTHOW($module);
                        }
                    }
                }
            ]
        ]
        <.ws>
    }
    
    sub do_import($module, $arglist) {
        if pir::exists($module, 'EXPORT') {
            my $EXPORT := $module<EXPORT>.WHO;
            if pir::exists($EXPORT, 'DEFAULT') {
                $*ST.import($EXPORT<DEFAULT>);
            }
        }
    }

    rule statement_control:sym<require> {
        <sym>
        [
        | <module_name> <EXPR>?
        | <EXPR>
        ]
    }

    token statement_control:sym<given> {
        <sym> <.end_keyword> :s <xblock(1)>
    }
    token statement_control:sym<when> {
        <sym> <.end_keyword> :s <xblock>
    }
    rule statement_control:sym<default> {
        <sym><.end_keyword> <block>
    }

    rule statement_control:sym<CATCH> {<sym> <block(1)> }
    rule statement_control:sym<CONTROL> {<sym> <block(1)> }

    proto token statement_prefix { <...> }
    token statement_prefix:sym<BEGIN> { <sym> <blorst> }
    token statement_prefix:sym<CHECK> { <sym> <blorst> }
    token statement_prefix:sym<INIT>  { <sym> <blorst> }
    token statement_prefix:sym<END>   { <sym> <blorst> }
    token statement_prefix:sym<FIRST> { <sym> <blorst>
            <.panic("FIRST phaser not yet implemented")> }
    token statement_prefix:sym<LAST>  { <sym> <blorst>
            <.panic("LAST phaser not yet implemented")> }
    token statement_prefix:sym<ENTER> { <sym> <blorst>
            <.panic("ENTER phaser not yet implemented")> }
    token statement_prefix:sym<LEAVE> { <sym> <blorst>
            <.panic("LEAVE phaser not yet implemented")> }
    token statement_prefix:sym<PRE> { <sym> <blorst>
            <.panic("PRE phaser not yet implemented")> }
    token statement_prefix:sym<POST> { <sym> <blorst>
            <.panic("POST phaser not yet implemented")> }
    token statement_prefix:sym<sink>  { <sym> <blorst> }
    token statement_prefix:sym<try>   { <sym> <blorst> }
    token statement_prefix:sym<gather>{ <sym> <blorst> }
    token statement_prefix:sym<do>    { <sym> <blorst> }
    token statement_prefix:sym<DOC>   {
        <sym> \s <.ws> $<phase>=['BEGIN' || 'CHECK' || 'INIT']
        <blorst>
    }

    token blorst {
        \s <.ws> [ <?[{]> <block> | <statement> ]
    }

    ## Statement modifiers

    proto token statement_mod_cond { <...> }

    rule modifier_expr { <EXPR> }

    token statement_mod_cond:sym<if>     { <sym> <modifier_expr> }
    token statement_mod_cond:sym<unless> { <sym> <modifier_expr> }
    token statement_mod_cond:sym<when>   { <sym> <modifier_expr> }

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
    token term:sym<circumfix>          { <circumfix> }
    token term:sym<statement_prefix>   { <statement_prefix> }
    token term:sym<**>                 { <sym> <.panic('HyperWhatever (**) not yet implemented')> }
    token term:sym<*>                  { <sym> }
    token term:sym<lambda>             { <?lambda> <pblock> }

    # XXX temporary Bool::True/Bool::False until we can get a permanent definition
    token term:sym<boolean> { 'Bool::'? $<value>=[True|False] » }

    token term:sym<undef> {
        <sym> >> {}
        [ <?before \h*'$/' >
            <.obs('$/ variable as input record separator',
                 "the filehandle's .slurp method")>
        ]?
        [ <?before [ '(' || \h*<sigil><twigil>?\w ] >
            <.obs('undef as a verb', 'undefine function or assignment of Nil')>
        ]?
        <.obs('undef as a value', "something more specific:\n\tMu (the \"most undefined\" type object),\n\tan undefined type object such as Int,\n\tNil as an empty list,\n\t!*.defined as a matcher or method,\n\tAny:U as a type constraint\n\tor fail() as a failure return\n\t   ")>
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
        | '(' ~ ')' <fakesignature>
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
            | <sigil> $<index>=[\d+] [ <?{ $*IN_DECL}> <.panic: "Cannot declare a numeric variable">]?
            | <sigil> <?[<[]> [ <?{ $*IN_DECL }> <.panic('Cannot declare a match variable')> ]?  <postcircumfix>
            | $<sigil>=['$'] $<desigilname>=[<[/_!]>]
            | <sigil> <?{ $*IN_DECL }>
            | <!{ $*QSIGIL }> <.panic("Non-declarative sigil is missing its name")>
            ]
        ]
        [ <?{ $<twigil> && $<twigil>[0] eq '.' }>
            [ <.unsp> | '\\' | <?> ] <?before '('> <arglist=.postcircumfix>
        ]?
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

    proto token package_declarator { <...> }
    token package_declarator:sym<package> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'package';
        <sym> <package_def> 
    }
    token package_declarator:sym<module> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'module';
        <sym> <package_def> 
    }
    token package_declarator:sym<class> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'class';
        <sym> <package_def> 
    }
    token package_declarator:sym<grammar> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'grammar';
        <sym> <package_def> 
    }
    token package_declarator:sym<role> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'role';
        <sym> <package_def> 
    }
    token package_declarator:sym<knowhow> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'knowhow';
        <sym> <package_def> 
    }
    token package_declarator:sym<native> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'native';
        <sym> <package_def> 
    }
    token package_declarator:sym<slang> {
        :my $*OUTERPACKAGE := $*PACKAGE;
        :my $*PKGDECL := 'slang';
        <sym> <package_def>
    }
    token package_declarator:sym<trusts> {
        <sym> <.ws> <typename>
    }
    token package_declarator:sym<also> {
        <sym>:s
        [ <trait>+ || <.panic: "No valid trait found after also"> ]
    }

    rule package_def {<.end_keyword>
        :my $longname;
        :my $outer := $*ST.cur_lexpad();
        :my $*DECLARAND;
        :my $*IN_DECL := 'package';
        :my $*CURPAD;
        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        <.attach_docs>
        
        # Meta-object will live in here; also set default REPR (a trait
        # may override this, e.g. is repr('...')).
        :my $*PACKAGE;
        :my $*REPR := 'P6opaque';
        
        # Default to our scoped.
        { unless $*SCOPE { $*SCOPE := 'our'; } }
        
        [
            [ <longname> { $longname := $<longname>[0]; } ]?
            <.newpad>
            
            [ #:dba('generic role')
            <?{ ($*PKGDECL//'') eq 'role' }>
            '[' ~ ']' <signature>
            { $*IN_DECL := ''; }
            ]?
            
            <trait>*
            
            {
                # Unless we're augmenting...
                if $*SCOPE ne 'augment' {
                    # Locate any existing symbol. Note that it's only a match
                    # with "my" if we already have a declaration in this scope.
                    my $exists := 0;
                    if $longname {
                        my @name := parse_name(~$longname<name>);
                        if $*ST.already_declared($*SCOPE, $*OUTERPACKAGE, $outer, @name) {
                            $*PACKAGE := $*ST.find_symbol(@name);
                            $exists := 1;
                        }
                    }

                    # If it exists already, then it's either uncomposed (in which
                    # case we just stubbed it), a role (in which case multiple
                    # variants are OK) or else an illegal redecl.
                    if $exists && $*PKGDECL ne 'role' {
                        if $*PACKAGE.HOW.is_composed($*PACKAGE) {
                            $/.CURSOR.panic("Illegal redeclaration of $*PKGDECL '" ~
                                ~$longname<name> ~ "'");
                        }
                    }
                    
                    # If it's not a role, or it is a role but one with no name,
                    # then just needs meta-object construction and installation.
                    elsif $*PKGDECL ne 'role' || !$longname {
                        # Construct meta-object for this package.
                        my %args;
                        if $longname {
                            %args<name> := ~$longname<name>;
                        }
                        %args<repr> := $*REPR;
                        $*PACKAGE := $*ST.pkg_create_mo(%*HOW{$*PKGDECL}, |%args);
                        
                        # Install it in the symbol table if needed.
                        if $longname {
                            $*ST.install_package_longname($/, $longname, $*SCOPE,
                                $*PKGDECL, $*OUTERPACKAGE, $outer, $*PACKAGE);
                        }
                    }
                    
                    # If it's a named role, a little trickier. We need to make
                    # a parametric role group for it (unless we got one), and
                    # then install it in that.
                    else {
                        # If the group doesn't exist, create it.
                        my $group;
                        if $exists {
                            $group := $*PACKAGE;
                        }
                        else {
                            $group := $*ST.pkg_create_mo(%*HOW{'role-group'}, :name(~$longname<name>));                            
                            $*ST.install_package_longname($/, $longname, $*SCOPE,
                                $*PKGDECL, $*OUTERPACKAGE, $outer, $group);
                        }

                        # Construct role meta-object with group.
                        $*PACKAGE := $*ST.pkg_create_mo(%*HOW{$*PKGDECL}, :name(~$longname<name>),
                            :group($group), :signatured($<signature> ?? 1 !! 0));
                    }
                }
                else {
                    # Augment. Ensure we can.
                    unless $*MONKEY_TYPING {
                        $/.CURSOR.panic("augment not allowed without 'use MONKEY_TYPING'");
                    }
                    if $*PKGDECL eq 'role' {
                        $/.CURSOR.panic("Can not augment a role, since roles are immutable");
                    }
                    unless $longname {
                        $/.CURSOR.panic("Can not augment the anonymous");
                    }
                    
                    # Locate type.
                    my $found;
                    my @name := parse_name(~$longname<name>);
                    try { $*PACKAGE := $*ST.find_symbol(@name); $found := 1 }
                    unless $found {
                        $/.CURSOR.panic("Could not find a $*PKGDECL " ~
                            ~$longname<name> ~ " to augment");
                    }
                }
                
                # Install $?PACKAGE, $?ROLE, $?CLASS, ::?CLASS as needed.
                my $curpad := $*ST.cur_lexpad();
                unless $curpad.symbol('$?PACKAGE') {
                    $*ST.install_lexical_symbol($curpad, '$?PACKAGE', $*PACKAGE);
                    if $*PKGDECL eq 'class' || $*PKGDECL eq 'grammar' {
                        $*ST.install_lexical_symbol($curpad, '$?CLASS', $*PACKAGE);
                        $*ST.install_lexical_symbol($curpad, '::?CLASS', $*PACKAGE);
                    }
                    elsif $*PKGDECL eq 'role' {
                        $*ST.install_lexical_symbol($curpad, '$?ROLE', $*PACKAGE);
                        $*ST.install_lexical_symbol($curpad, '$?CLASS',
                            $*ST.pkg_create_mo(%*HOW<generic>, :name('$?CLASS')));
                        $*ST.install_lexical_symbol($curpad, '::?CLASS',
                            $*ST.pkg_create_mo(%*HOW<generic>, :name('::?CLASS')));
                    }
                }
                
                # Set declarand as the package.
                $*DECLARAND := $*PACKAGE;
                
                # Apply any traits.
                for $<trait> {
                    my $applier := $_.ast;
                    if $applier {
                        $applier($*DECLARAND);
                    }
                }
            }
            
            [
            || <?[{]> 
                [
                {
                    $*IN_DECL := '';
                    $*begin_compunit := 0;
                }
                <blockoid>
                ]
            
            || ';'
                [
                || <?{ $*begin_compunit }>
                    {
                        unless $longname {
                            $/.CURSOR.panic("Compilation unit cannot be anonymous");
                        }
                        unless $outer =:= $*UNIT {
                            $/.CURSOR.panic("Semicolon form of " ~ $*PKGDECL ~ " definition not allowed in subscope;\n  please use block form");
                        }
                        if $*PKGDECL eq 'package' {
                            $/.CURSOR.panic('This appears to be Perl 5 code. If you intended it to be Perl 6 code, please use a Perl 6 style package block like "package Foo { ... }", or "module Foo; ...".');
                        }
                        $*begin_compunit := 0;
                    }
                    { $*IN_DECL := ''; }
                    <.finishpad>
                    <statementlist>     # whole rest of file, presumably
                    { $*CURPAD := $*ST.pop_lexpad() }
                || <.panic: "Too late for semicolon form of $*PKGDECL definition">
                ]
            || <.panic: "Unable to parse $*PKGDECL definition">
            ]
        ] || <.panic: "Malformed $*PKGDECL">
    }

    token declarator {
        [
        | <variable_declarator>
        | '(' ~ ')' <signature> <trait>*
        | <routine_declarator>
        | <regex_declarator>
        | <type_declarator>
        ]
    }

    proto token multi_declarator { <...> }
    token multi_declarator:sym<multi> {
        <sym> :my $*MULTINESS := 'multi'; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.panic: 'Malformed multi'> ]
    }
    token multi_declarator:sym<proto> {
        <sym> :my $*MULTINESS := 'proto'; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.panic: 'Malformed proto'> ]
    }
    token multi_declarator:sym<only> {
        <sym> :my $*MULTINESS := 'only'; <.end_keyword>
        <.ws> [ <declarator> || <routine_def('sub')> || <.panic: 'Malformed only'> ]
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
    token scope_declarator:sym<state>     { <sym> <scoped('state')> }
    token scope_declarator:sym<supersede> {
        <sym> <scoped('supersede')> <.panic: '"supersede" not yet implemented'>
    }

    rule scoped($*SCOPE) {<.end_keyword> [
        :my $*TYPENAME := '';

        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        <.attach_docs>
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
            $*TYPENAME := $<typename>[0];
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
    ] }

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
            ]+
            <.panic: "Shaped variable declarations are not yet implemented">
        ]?
        <trait>*
    }

    proto token routine_declarator { <...> }
    token routine_declarator:sym<sub>
        { <sym> <.end_keyword> <routine_def('sub')> }
    token routine_declarator:sym<method>
        { <sym> <.end_keyword> <method_def('method')> }
    token routine_declarator:sym<submethod>
        { <sym> <.end_keyword> <method_def('submethod')> }
    token routine_declarator:sym<macro>
        { <sym> <.end_keyword>
          <.panic: "Macros are not yet implemented"> }

    rule routine_def($d) {
        :my $*IN_DECL := $d;
        :my $*METHODTYPE;
        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        <.attach_docs>
        <deflongname>?
        <.newpad>
        [ '(' <multisig> ')' ]?
        <trait>*
        { $*IN_DECL := ''; $*IMPLICIT := 0; }
        [
        | <onlystar>
        | <blockoid>
        ]
    }

    rule method_def($d) {
        :my $*IN_DECL := $d;
        :my $*METHODTYPE := $d;
        :my $*DOC := $*DECLARATOR_DOCS;
        :my $*DOCEE;
        <.attach_docs>
        [
            <.newpad>
            [
                | $<specials>=[<[ ! ^ ]>?]<longname> [ '(' <multisig> ')' ]? <trait>*
                | [ '(' <multisig> ')' ]? <trait>*
                | <?>
            ]
            { $*IN_DECL := ''; }
            [
            | <onlystar>
            | <blockoid>
            ]
        ] || <.panic: 'Malformed method'>
    }
    
    token onlystar {
        :my $*CURPAD;
        <?{ $*MULTINESS eq 'proto' }>
        '{' <.ws> '*' <.ws> '}'
        <?ENDSTMT>
        <.finishpad>
        { $*CURPAD := $*ST.pop_lexpad() }
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

    token fakesignature {
        <.newpad>
        <signature>
        { $*ST.pop_lexpad() }
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
        # We'll collect parameter information into a hash, then use it to
        # build up the parameter object in the action method
        :my %*PARAM_INFO;
        [
        | <type_constraint>+
            [
            | $<quant>=['**'|'*'|'\\'|'|'] <param_var>
            | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
            | <?>
            ]
        | $<quant>=['**'|'*'|'\\'|'|'] <param_var>
        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
        | <longname> <.panic('Invalid typename in parameter declaration')>
        ]
        <trait>*
        <post_constraint>*
        <default_value>?

        # enforce zone constraints
        {
            my $kind :=
                $<named_param>                      ?? '*' !!
                $<quant> eq '?' || $<default_value> ?? '?' !!
                $<quant> eq '!'                     ?? '!' !!
                $<quant> ne '' && $<quant> ne '\\'  ?? '*' !!
                                                       '!';
            if $kind eq '!' {
                if $*zone eq 'posopt' {
                    $/.CURSOR.panic("Cannot put required parameter after optional parameters");
                }
                elsif $*zone eq 'var' {
                    $/.CURSOR.panic("Cannot put required parameter after variadic parameters");
                }
            }
            elsif $kind eq '?' {
                if $*zone  eq 'posreq' {
                        $*zone := 'posopt';
                }
                elsif $*zone eq  'var' {
                    $/.CURSOR.panic("Cannot put optional positional parameter after variadic parameters");
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
          || <name=.decint> <.panic: "Cannot declare a numeric parameter">
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
        <sym>
        :my %*RX;
        :my $*METHODTYPE := 'rule';
        {
            %*RX<s> := 1;
            %*RX<r> := 1;
        }
        <regex_def>
    }
    token regex_declarator:sym<token> {
        <sym>
        :my %*RX;
        :my $*METHODTYPE := 'token';
        {
            %*RX<r> := 1;
        }
        <regex_def>
    }
    token regex_declarator:sym<regex> {
        <sym>
        :my %*RX;
        :my $*METHODTYPE := 'regex';
        <regex_def>
    }

    rule regex_def {<.end_keyword> [
        :my $*CURPAD;
        [
          { $*IN_DECL := '' }
          <deflongname>?
          <.newpad>
          [ [ ':'?'(' <signature> ')'] | <trait> ]*
          '{'[ '<...>' |<p6regex=.LANG('Regex','nibbler')>]'}'<?ENDSTMT>
          { $*CURPAD := $*ST.pop_lexpad() }
        ] || <.panic: "Malformed regex">
    ] }

    proto token type_declarator { <...> }

    token type_declarator:sym<enum> {
        :my $*IN_DECL := 'enum';
        :my $*DECLARAND;
        <sym> <.ws>
        [
        | <longname>
        | <variable>
        | <?>
        ]
        { $*IN_DECL := ''; }
        <.ws>
        <trait>*
        <?before <[ < ( « ]> > <term> <.ws>
    }

    token type_declarator:sym<subset> {
        <sym> :my $*IN_DECL := 'subset';
        <.end_keyword>
        :s
        [
            [
                [ <longname> ]?
                { $*IN_DECL := '' }
                <trait>*
                [ where <EXPR('e=')> ]?
            ]
            || <.panic: 'Malformed subset'>
        ]
    }

    token type_declarator:sym<constant> {
        :my $*IN_DECL := 'constant';
        <sym> <.end_keyword> <.ws>

        [
        | <identifier>
        | <variable>
        | <?>
        ]
        { $*IN_DECL := ''; }
        <.ws>

        <trait>*

        [
        || <?before '='>
        || <?before <-[\n=]>*'='> <.panic: "Malformed constant"> # probable initializer later
        || <.panic: "Missing initializer on constant declaration">
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
    token trait_mod:sym<hides>   { <sym>:s <typename> }
    token trait_mod:sym<does>    { <sym>:s <typename> }
    token trait_mod:sym<will>    { <sym>:s <identifier> <pblock> }
    token trait_mod:sym<of>      { <sym>:s <typename> }
    token trait_mod:sym<as>      { <sym>:s <typename> }
    token trait_mod:sym<returns> { <sym>:s <typename> }
    token trait_mod:sym<handles> { <sym>:s <term> }


    ## Terms

    proto token term { <...> }

    token term:sym<self> { <sym> <.end_keyword> }

    token term:sym<now> { <sym> <.end_keyword> }

    token term:sym<time> { <sym> <.end_keyword> }

    token term:sym<rand> {
        <sym> »
        <.end_keyword>
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
                pir::substr($longname, 0, 2) eq '::' || $*ST.is_name(parse_name($longname))
            }>
            <.unsp>? [ <?before '['> '[' ~ ']' <arglist> ]?
        || <args>
        ]
    }

    token term:sym<pir::op> {
        'pir::' $<op>=[\w+] <args>?
    }

    token term:sym<pir::const> {
        'pir::const::' $<const>=[\w+]
    }

    token term:sym<nqp::op> {
        'nqp::' $<op>=[\w+] <args>?
    }

    token term:sym<dotty> { <dotty> }

    token term:sym<capterm> { <capterm> }
    
    token term:sym<onlystar> {
        '{*}' <?ENDSTMT>
        [ <?{ $*MULTINESS eq 'proto' }> || <.panic: '{*} may only appear in proto'> ]
    }

    token args {
        | '(' <semiarglist> ')'
        | [ \s <arglist> ]
        | <?>
    }

    token semiarglist {
        <arglist> ** ';'
        <.ws>
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
        | '::?'<identifier> <colonpair>*    # parse ::?CLASS as special case
        | <longname>
          <?{
            my $longname := canonical_type_longname($<longname>);
            pir::substr($longname, 0, 2) eq '::' ??
                1 !! # ::T introduces a type, so always is one
                $*ST.is_name(parse_name($longname))
          }>
        ]
        # parametric type?
        <.unsp>? [ <?before '['> '[' ~ ']' <arglist> ]?
        [<.ws> 'of' <.ws> <typename> ]?
    }
    
    our sub canonical_type_longname($/) {
        my $ver := '';
        my $auth := '';
        for $<colonpair> {
            if $<identifier> && $<circumfix> {
                if $<identifier>.Str eq 'ver' {
                    $ver := $<circumfix>.Str;
                }
                elsif $<identifier>.Str eq 'auth' {
                    $auth := $<circumfix>.Str;
                }
            }
        }
        return ~$<name> ~ $ver ~ $auth;
    }

    token term:sym<type_declarator>   { <type_declarator> }

    token quotepair {
        :my $*key;
        :my $*value;
        ':'
        # :dba('restricted colonpair')
        [
        | '!' <identifier> [ <?before '('> <.panic('Argument not allowed on negated pair')> ]?
            { $*key := ~$<identifier>; $*value := 0; }
        | <identifier> 
            { $*key := ~$<identifier> }
            [
            || <?before '('> <circumfix> { $*value := $<circumfix>.ast; }
            || { $*value := 1; }
            ]
        | (\d+) <identifier>
            [ <?before '('> <.cirumfix> <.panic('2nd argument not allowed on pair')> ]?
            { $*key := ~$<identifier>; $*value := +~$/[0] }
        ]
    }

    token rx_adverbs {
        [
            <quotepair> <.ws>
            :my $*ADVERB;
            { $*ADVERB := $<quotepair>[-1] }
            <.setup_quotepair>
        ]*
    }

    proto token quote { <...> }
    token quote:sym<apos>  { <?[']>                <quote_EXPR: ':q'>  }
    token quote:sym<dblq>  { <?["]>                <quote_EXPR: ':qq'> }
    token quote:sym<q>     { 'q'   >> <![(]> <.ws> <quote_EXPR: ':q'>  }
    token quote:sym<qq>    { 'qq'  >> <![(]> <.ws> <quote_EXPR: ':qq'> }
    token quote:sym<qw>    { 'qw'  >> <![(]> <.ws> <quote_EXPR: ':q',':w'> }
    token quote:sym<qx>    { 'qx'  >> <![(]> <.ws> <quote_EXPR: ':q'>  }
    token quote:sym<qqx>   { 'qqx' >> <![(]> <.ws> <quote_EXPR: ':qq'> }
    token quote:sym<Q>     { 'Q'   >> <![(]> <.ws> <quote_EXPR> }
    token quote:sym<Q:PIR> { 'Q:PIR'      <.ws> <quote_EXPR> }
    token quote:sym</null/> { '/' \s* '/' <.panic: "Null regex not allowed"> }
    token quote:sym</ />  { '/' :my %*RX; <p6regex=.LANG('Regex','nibbler')> '/' <.old_rx_mods>? }
    token quote:sym<rx>   {
        <sym> >> 
        :my %*RX;
        <rx_adverbs>
        [
        | '/'<p6regex=.LANG('Regex','nibbler')>'/' <.old_rx_mods>?
        | '{'<p6regex=.LANG('Regex','nibbler')>'}' <.old_rx_mods>?
        ]
    }

    method match_with_adverb($v) {
        my $s := Regex::Match.new();
        $s.'!make'(PAST::Val.new(:value(1), :named('s')));
        $s;
    }

    token quote:sym<m> {
        <sym> (s)?>>
        :my %*RX;
        <rx_adverbs>
        [
        | '/'<p6regex=.LANG('Regex','nibbler')>'/' <.old_rx_mods>?
        | '{'<p6regex=.LANG('Regex','nibbler')>'}'
        ]
    }

    token setup_quotepair { '' }

    token quote:sym<s> {
        <sym> (s)? >>
        :my %*RX;
        {
            %*RX<s> := 1 if $/[0]
        }
        <rx_adverbs>
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
            elsif $m eq 'm' { $/.CURSOR.obs('/m','^^ and $$ anchors');                    }
            elsif $m eq 's' { $/.CURSOR.obs('/s','. or \N');                              }
            elsif $m eq 'x' { $/.CURSOR.obs('/x','normal default whitespace');            }
            elsif $m eq 'c' { $/.CURSOR.obs('/c',':c or :p');                             }
            elsif $m eq 'e' { $/.CURSOR.obs('/e','interpolated {...} or s{} = ... form'); }
            else            { $/.CURSOR.obs('suffix regex modifiers','prefix adverbs');   }
        }
    }

    token quote_escape:sym<$> {
        <?[$]>
        :my $*QSIGIL := '$';
        <?quotemod_check('s')>
        [ <EXPR('y=')> || <.panic: "Non-variable \$ must be backslashed"> ]
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
        [ <?before '<STDIN>' > <.obs('<STDIN>', '$*IN.lines (or add whitespace to suppress warning)')> ]?
        [ <?before '<>' > <.obs('<>', 'lines() to read input, (\'\') to represent a null string or () to represent an empty list')> ]?
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
        Perl6::Grammar.O(':prec<m=>, :assoc<left>, :iffy<1>, :pasttype<chain>',  '%chaining');
        Perl6::Grammar.O(':prec<l=>, :assoc<left>',  '%tight_and');
        Perl6::Grammar.O(':prec<k=>, :assoc<list>',  '%tight_or');
        Perl6::Grammar.O(':prec<j=>, :assoc<right>', '%conditional');
        Perl6::Grammar.O(':prec<i=>, :assoc<right>', '%item_assignment');
        Perl6::Grammar.O(':prec<i=>, :assoc<right>, :sub<e=>', '%list_assignment');
        Perl6::Grammar.O(':prec<h=>, :assoc<unary>', '%loose_unary');
        Perl6::Grammar.O(':prec<g=>, :assoc<list>, :nextterm<nulltermish>',  '%comma');
        Perl6::Grammar.O(':prec<f=>, :assoc<list>',  '%list_infix');
        Perl6::Grammar.O(':prec<e=>, :assoc<right>', '%list_prefix');
        Perl6::Grammar.O(':prec<d=>, :assoc<left>',  '%loose_and');
        Perl6::Grammar.O(':prec<c=>, :assoc<list>',  '%loose_or');
        Perl6::Grammar.O(':prec<b=>, :assoc<list>',  '%sequencer');
    }

    token termish {
        :my $*SCOPE := "";
        :my $*MULTINESS := "";
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
        my $last  := pir::substr($str, nqp::chars($check) - 1, 1);
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
        $<opening>=[ '«' | '»' ]
        {} <infixish>
        $<closing>=[ '«' | '»' || <.panic("Missing « or »")> ]
        <O=.copyO('infixish')>
    }

    token infix_circumfix_meta_operator:sym«<< >>» {
        $<opening>=[ '<<' | '>>' ]
        {} <infixish>
        $<closing>=[ '<<' | '>>' || <.panic("Missing << or >>")> ]
        <O=.copyO('infixish')>
    }

    method copyO($from) {
        my $m := self.MATCH();
        my $r := $m{$from}<OPER><O>;
        self.'!cursor_start'().'!cursor_pass'(self.pos(), '', $r);
    }

    method copyOPER($from) {
        my $m := self.MATCH();
        my $r := $m{$from}<OPER>;
        self.'!cursor_start'().'!cursor_pass'(self.pos(), '', $r);
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
            [
            | <?[(]> <args>
            | ':' \s <!{ $*QSIGIL }> <args=.arglist>
            ]
            || <!{ $*QSIGIL }> <?>
            || <?{ $*QSIGIL }> <?['.']> <?>
        ]
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
    token prefix:sym<~^>  { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<^>   { <sym>  <O('%symbolic_unary')> }
    token prefix:sym<|>   { <sym>  <O('%symbolic_unary')> }

    token infix:sym<*>    { <sym>  <O('%multiplicative')> }
    token infix:sym</>    { <sym>  <O('%multiplicative')> }
    token infix:sym<div>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<gcd>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<lcm>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<%>    { <sym>  <O('%multiplicative')> }
    token infix:sym<mod>  { <sym> >> <O('%multiplicative')> }
    token infix:sym<%%>   { <sym>  <O('%multiplicative, :iffy<1>')> }
    token infix:sym<+&>   { <sym>  <O('%multiplicative')> }
    token infix:sym<~&>   { <sym>  <O('%multiplicative')> }
    token infix:sym<?&>   { <sym>  <O('%multiplicative')> }
    token infix:sym«+<»   { <sym> <!before '<'> <O('%multiplicative')> }
    token infix:sym«+>»   { <sym> <!before '>'> <O('%multiplicative')> }

    token infix:sym«<<» { <sym> \s <.obs('<< to do left shift', '+< or ~<')> }

    token infix:sym«>>» { <sym> \s <.obs('>> to do right shift', '+> or ~>')> }

    token infix:sym<+>    { <sym>  <O('%additive')> }
    token infix:sym<->    {
       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
        <sym> [<?before '>>'> || <!before '>'>]
        <O('%additive')>
    }
    token infix:sym<+|>   { <sym>  <O('%additive')> }
    token infix:sym<+^>   { <sym>  <O('%additive')> }
    token infix:sym<~|>   { <sym>  <O('%additive')> }
    token infix:sym<~^>   { <sym>  <O('%additive')> }
    token infix:sym<?|>   { <sym>  <O('%additive')> }
    token infix:sym<?^>   { <sym>  <O('%additive')> }

    token infix:sym<x>    { <sym> >> <O('%replication')> }
    token infix:sym<xx>    { <sym> >> <O('%replication')> }

    token infix:sym<~>    { <sym>  <O('%concatenation')> }

    token infix:sym<&>    { <sym> <O('%junctive_and')> }
    token infix:sym<|>    { <sym> <O('%junctive_or')> }
    token infix:sym<^>    { <sym> <O('%junctive_or')> }

    token prefix:sym<abs>     { <sym> » <O('%named_unary')> }
    token prefix:sym<defined> { <sym> » <O('%named_unary')> }

    token infix:sym«==»   { <sym>  <O('%chaining')> }
    token infix:sym«!=»   { <sym> <?before \s|']'> <O('%chaining')> }
    token infix:sym«<=»   { <sym>  <O('%chaining')> }
    token infix:sym«>=»   { <sym>  <O('%chaining')> }
    token infix:sym«<»    { <sym>  <O('%chaining')> }
    token infix:sym«>»    { <sym>  <O('%chaining')> }
    token infix:sym«eq»   { <sym> >> <O('%chaining')> }
    token infix:sym«ne»   { <sym> >> <O('%chaining')> }
    token infix:sym«le»   { <sym> >> <O('%chaining')> }
    token infix:sym«ge»   { <sym> >> <O('%chaining')> }
    token infix:sym«lt»   { <sym> >> <O('%chaining')> }
    token infix:sym«gt»   { <sym> >> <O('%chaining')> }
    token infix:sym«=:=»  { <sym>  <O('%chaining')> }
    token infix:sym<===>  { <sym>  <O('%chaining')> }
    token infix:sym<eqv>  { <sym> >> <O('%chaining')> }
    token infix:sym<before>  { <sym> >> <O('%chaining')> }
    token infix:sym<after>  { <sym>>>  <O('%chaining')> }
    token infix:sym<~~>   { <sym>  <O('%chaining')> <!dumbsmart> }
    token infix:sym<!~~>  { <sym>  <O('%chaining')> <!dumbsmart> }

    token dumbsmart {
        # should be
        # 'Bool::'? True && <.longname>
        # once && in regexes is implemented
        | <?before \h* [ 'Bool::'? 'True' && <.longname> ] >  <.panic("Smartmatch against True always matches; if you mean to test the topic for truthiness, use :so or *.so or ?* instead")>
        | <?before \h* [ 'Bool::'? 'False' && <.longname> ] > <.panic("Smartmatch against False always fails; if you mean to test the topic for truthiness, use :!so or *.not or !* instead")>
    }

    token infix:sym<&&>   { <sym>  <O('%tight_and, :pasttype<if>')> }

    token infix:sym<||>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<unless>')> }
    token infix:sym<^^>   { <sym>  <O('%tight_or, :pasttype<xor_nqp>')> }
    token infix:sym<//>   { <sym>  <O('%tight_or, :assoc<left>, :pasttype<def_or>')> }
    token infix:sym<min>  { <sym> >> <O('%tight_or')> }
    token infix:sym<max>  { <sym> >> <O('%tight_or')> }

    token infix:sym<?? !!> {
        '??'
        <.ws>
        <EXPR('i=')>
        [ '!!'
        || <?before '::'<-[=]>> <.panic: "Please use !! rather than ::">
        || <?before ':' <-[=]>> <.panic: "Please use !! rather than :">
        || <?before \N*? [\n\N*?]?> '!!' <.panic("Bogus code found before the !!")>
        || <.panic("Found ?? but no !!")>
        ]
        <O('%conditional, :reducecheck<ternary>, :pasttype<if>')>
    }

    token infix_prefix_meta_operator:sym<!> {
        <sym> <infixish> 
        [
        || <?{ $<infixish>.Str eq '=' }> <O('%chaining')>
        || <?{ $<infixish><OPER><O><iffy> }> <O=.copyO('infixish')>
        || <.panic("Cannot negate " ~ $<infixish>.Str ~ " because it is not iffy enough")>
        ]
    }
    token infix_prefix_meta_operator:sym<R> { <sym> <infixish> <O=.copyO('infixish')> }
    token infix_prefix_meta_operator:sym<S> { <sym> <infixish> <O=.copyO('infixish')> }
    token infix_prefix_meta_operator:sym<X> { <sym> <infixish> <O('%list_infix')> }
    token infix_prefix_meta_operator:sym<Z> { <sym> <infixish> <O('%list_infix')> }
    token infix:sym<minmax> { <sym> >> <O('%list_infix')> }

    token infix:sym<:=> {
        <sym>  <O('%list_assignment')>
    }

    token infix:sym<::=> {
        <sym>  <O('%item_assignment')>
    }

    token infix:sym<.=> { <sym> <O('%item_assignment, :nextterm<dottyopish>')> }

    # Should probably have <!after '='> to agree w/spec, but after NYI.
    # Modified infix != below instead to prevent misparse
    token infix_postfix_meta_operator:sym<=> { '=' <O('%item_assignment')> }

    token infix:sym«=>» { <sym> <O('%item_assignment')> }

    token prefix:sym<so> { <sym> >> <O('%loose_unary')> }
    token prefix:sym<not>  { <sym> >> <O('%loose_unary')> }

    token infix:sym<,>    {
        <sym>  <O('%comma')>
        # TODO: should be <.worry>, not <.panic>
        [ <?before \h*'...'> <.panic: "Comma found before apparent series operator; please remove comma (or put parens\n    around the ... listop, or use 'fail' instead of ...)"> ]?
    }

    token infix:sym<Z>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }
    token infix:sym<X>    { <!before <sym> <infixish> > <sym>  <O('%list_infix')> }

    token infix:sym<...>  { <sym>  <O('%list_infix')> }
    token infix:sym<...^> { <sym>  <O('%list_infix')> }
    # token term:sym<...>   { <sym> <args>? <O(|%list_prefix)> }

    token infix:sym<?>    { <sym> {} <!before '?'> <?before <-[;]>*?':'> <.obs('?: for the conditional operator', '??!!')> <O('%conditional')> }

    token infix:sym<ff> { <sym> <.panic('Flip flip operators are not yet implemented')> }
    token infix:sym<^ff> { <sym> <.panic('Flip flip operators are not yet implemented')> }
    token infix:sym<ff^> { <sym> <.panic('Flip flip operators are not yet implemented')> }
    token infix:sym<^ff^> { <sym> <.panic('Flip flip operators are not yet implemented')> }

    token infix:sym<=> {
        <sym>
        [
        || <?{ $*LEFTSIGIL eq '$' }> <O('%item_assignment')>
        || <O('%list_assignment')>
        ]
    }

    token infix:sym<and>  { <sym> >> <O('%loose_and, :pasttype<if>')> }

    token infix:sym<or>   { <sym> >> <O('%loose_or, :assoc<left>, :pasttype<unless>')> }
    token infix:sym<xor>  { <sym> >> <O('%loose_or, :pasttype<xor_nqp>')> }
    token infix:sym<orelse> { <sym> >> <O('%loose_or, :assoc<left>, :pasttype<def_or>')> }

    token infix:sym«<==»  { <sym> <O('%sequencer')> }
    token infix:sym«==>»  { <sym> <O('%sequencer')> }
    token infix:sym«<<==» { <sym> <O('%sequencer')> }
    token infix:sym«==>>» { <sym> <O('%sequencer')> }

    token infix:sym<..>   { <sym> <O('%structural')> }
    token infix:sym<^..>  { <sym> <O('%structural')> }
    token infix:sym<..^>  { <sym> <O('%structural')> }
    token infix:sym<^..^> { <sym> <O('%structural')> }

    token infix:sym<leg>  { <sym> >> <O('%structural')> }
    token infix:sym<cmp>  { <sym> >> <O('%structural')> }
    token infix:sym«<=>»  { <sym> <O('%structural')> }

    token infix:sym<but>  { <sym> >> <O('%structural')> }
    token infix:sym<does> { <sym> >> <O('%structural')> }

    token infix:sym<!~> { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> <O('%chaining')> }
    token infix:sym<=~> { <sym> <.obs('=~ to do pattern matching', '~~')> <O('%chaining')> }

    our %is_sigil;
    INIT {
        our %is_sigil;
        %is_sigil{'$'} := 1;
        %is_sigil{'@'} := 1;
        %is_sigil{'%'} := 1;
        %is_sigil{'&'} := 1;
    }

    our sub parse_name($name) {
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


    # This method is used to augment the grammar with new ops at parse time.
    method gen_op($category, $opname, $canname, $subname) {
        my $self := self;

        # Work out what default precedence we want, or if it's more special than
        # just an operator.
        my $prec;
        my $is_oper;
        if $category eq 'infix' {
            $prec := '%additive';
            $is_oper := 1;
        }
        elsif $category eq 'prefix' {
            $prec := '%symbolic_unary';
            $is_oper := 1;
        }
        elsif $category eq 'postfix' {
            $prec := '%autoincrement';
            $is_oper := 1;
        }
        elsif $category eq 'circumfix' {
            $is_oper := 0;
        }
        elsif $category eq 'trait_mod' {
            return 0;
        }
        else {
            pir::die("Cannot add tokens of category '$category' with a sub");
        }

        # Nope, so we need to modify the grammar. Build code to parse it.
        my $parse := PAST::Regex.new(
            :pasttype('concat')
        );
        if $is_oper {
            # For operator, it's just like 'op' <O('%prec')>
            $parse.push(PAST::Regex.new(
                :pasttype('subcapture'),
                :name('sym'),
                :backtrack('r'),
                PAST::Regex.new(
                    :pasttype('literal'),
                    $opname
                )
            ));
            $parse.push(PAST::Regex.new(
                :pasttype('subrule'),
                :name('O'),
                :backtrack('r'),
                'O',
                PAST::Val.new( :value($prec) )
            ));
        }
        else {
            # Find opener and closer and parse an EXPR between them.
            # XXX One day semilist would be nice, but right now that
            # runs us into fun with terminators.
            my @parts := nqp::split(' ', $opname);
            if +@parts != 2 {
                pir::die("Unable to find starter and stopper from '$opname'");
            }
            $parse.push(PAST::Regex.new(
                :pasttype('literal'), :backtrack('r'),
                ~@parts[0]
            ));
            $parse.push(PAST::Regex.new(
                :pasttype('concat'),
                PAST::Regex.new(
                    :pasttype('subrule'), :subtype('capture'), :backtrack('r'),
                    :name('EXPR'), 'EXPR'
                ),
                PAST::Regex.new(
                    :pasttype('literal'), :backtrack('r'),
                    ~@parts[1]
                )
            ));
        }
        $parse := Regex::P6Regex::Actions::buildsub($parse);

        # Needs to go into the Perl6::Grammar namespace.
        $parse.name($canname);
        $parse.namespace(pir::split('::', 'Perl6::Grammar'));

        # Compile and then install the two produced methods into the
        # Perl6::Grammar methods table.
        my $compiled := PAST::Compiler.compile($parse);
        $self.HOW.add_method($self, ~$compiled[0], $compiled[0]);
        $self.HOW.add_method($self, ~$compiled[1], $compiled[1]);

        # May also need to add to the actions.
        if $category eq 'circumfix' {
            Perl6::Actions.HOW.add_method(Perl6::Actions, $canname, sub ($self, $/) {
                make PAST::Op.new(
                    :pasttype('call'), :name($subname),
                    $<EXPR>.ast
                );
            });
        }

        # Mark proto-regex table as needing re-generation.
        Q:PIR {
            $P0 = find_lex '$self'
            $P0.'!protoregex_generation'()
        };

        return 1;
    }
}

grammar Perl6::RegexGrammar is QRegex::P6Regex::Grammar {
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
