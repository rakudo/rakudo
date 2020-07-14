use NQPP6QRegex;
use NQPP5QRegex;

# The AST nodes come from the Raku setting bootstrap, thus we need to load
# them from there.
my $ast_root;
sub ensure_raku_ast() {
    unless nqp::isconcrete($ast_root) {
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $unit := $loader.load_module('Perl6::BOOTSTRAP::v6c', {}, GLOBALish);
        my $export := $unit<EXPORT>.WHO<DEFAULT>.WHO;
        $ast_root := nqp::existskey($export, 'RakuAST')
                ?? nqp::atkey($export, 'RakuAST').WHO
                !! nqp::die('Cannot find RakuAST nodes');
    }
}

role Raku::CommonActions {
    method quibble($/) {
        make $<nibble>.ast;
    }
}

class Raku::Actions is HLL::Actions does Raku::CommonActions {
    proto method r(*@parts) {*}
    multi method r($t) {
        nqp::ifnull(nqp::atkey($ast_root, $t), nqp::die("No such node RakuAST::{$t}"))
    }
    multi method r($t1, $t2) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}"))
    }
    multi method r($t1, $t2, $t3) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        $res := nqp::atkey($res.WHO, $t3) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}::{$t3}"))
    }

    ##
    ## Compilation unit, language version and other entry point bits
    ##

    # Used to ensure uniqueness of serialization context IDs.
    my class SerializationContextId {
        my $count := 0;
        my $lock  := NQPLock.new;
        method next-id() {
            $lock.protect({ $count++ })
        }
    }

    method lang_setup($/) {
        ensure_raku_ast();

        # Calculate the setting name to use.
        # TODO don't hardcode this
        my $name := 'CORE';
        my $version := nqp::substr(nqp::getcomp('Raku').config<language-version>, 2);
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting-name := $loader.transform_setting_name("$name.$version");

        # Set up the resolver.
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
        if nqp::isconcrete($outer_ctx) {
            $*R := $resolver_type.from-context(:context($outer_ctx));
        }
        else {
            $*R := $resolver_type.from-setting(:$setting-name);
        }

        # Locate an EXPORTHOW and set those mappings on our current language.
        my $EXPORTHOW := $*R.resolve-lexical-constant('EXPORTHOW').compile-time-value;
        for stash_hash($EXPORTHOW) {
            $*LANG.set_how($_.key, $_.value);
        }

        # Create a compilation unit.
        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isconcrete($outer_ctx) {
            # It's an EVAL. We'll take our GLOBAL, $?PACKAGE, etc. from that.
            my $comp-unit-name := nqp::sha1($file ~ $/.target() ~ SerializationContextId.next-id());
            $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name, :eval);
        }
        else {
            # Top-level compilation. Create a GLOBAL using the correct package meta-object.
            my $comp-unit-name := nqp::sha1($file ~ $/.target());
            $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name,
                :global-package-how($*LANG.how('package')));
        }

        # Set up the literals builder, so we can produce and intern literal
        # values.
        $*LITERALS := self.r('LiteralBuilder').new();
    }

    sub stash_hash($pkg) {
        my $hash := $pkg.WHO;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }

    method comp_unit($/) {
        # Put the body in place.
        my $cu := $*CU;
        $cu.replace-statement-list($<statementlist>.ast);

        # Sort out sinking; the compilation unit is sunk as a whole if we are
        # not in a REPL or EVAL context.
        $cu.mark-sunk() unless nqp::isconcrete(%*COMPILING<%?OPTIONS><outer_ctx>);
        $cu.calculate-sink();

        # Have check time.
        $cu.check($*R);

        make $cu;
    }

    ##
    ## Statements
    ##

    method statementlist($/) {
        my $list := self.r('StatementList').new();
        for $<statement> {
            $list.push($_.ast);
        }
        make $list;
    }

    method semilist($/) {
        my $list := self.r('SemiList').new();
        for $<statement> {
            $list.push($_.ast);
        }
        make $list;
    }

    method statement($/) {
        if $<EXPR> {
            make self.r('Statement', 'Expression').new($<EXPR>.ast);
        }
        elsif $<statement_control> {
            make $<statement_control>.ast;
        }
        else {
            nqp::die('unimpl statemnet type');
        }
    }

    method pblock($/) {
        my $block := $*BLOCK;
        if $<signature> {
            $block.replace-signature($<signature>.ast);
        }
        $block.replace-body($<blockoid>.ast);
        make $block;
    }

    method block($/) {
        my $block := $*BLOCK;
        $block.replace-body($<blockoid>.ast);
        make $block;
    }

    method blockoid($/) {
        make self.r('Blockoid').new($<statementlist>.ast);
    }

    method enter-block-scope($/) {
        my $block := self.r($*SCOPE-KIND).new;
        $*R.enter-scope($block);
        $*BLOCK := $block;
    }

    method leave-block-scope($/) {
        $*R.leave-scope();
    }

    method statement_control:sym<if>($/) {
        my $condition := $<condition>[0].ast;
        my $then := $<then>[0].ast;
        my @elsifs;
        my $index := 0;
        for $<sym> {
            if $index > 0 {
                my $elsif-type := $_ eq 'orwith' ?? 'Orwith' !! 'Elsif';
                @elsifs.push: self.r('Statement', $elsif-type).new:
                    condition => $<condition>[$index].ast,
                    then => $<then>[$index].ast;
            }
            $index++;
        }
        my $else := $<else> ?? $<else>.ast !! self.r('Block');
        make self.r('Statement', $<sym>[0] eq 'with' ?? 'With' !! 'If').new:
            :$condition, :$then, :@elsifs, :$else;
    }

    method statement_control:sym<unless>($/) {
        make self.r('Statement', 'Unless').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<without>($/) {
        make self.r('Statement', 'Without').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<while>($/) {
        make self.r('Statement', 'Loop', $<sym> eq 'while' ?? 'While' !! 'Until').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<repeat>($/) {
        make self.r('Statement', 'Loop', $<wu> eq 'while' ?? 'RepeatWhile' !! 'RepeatUntil').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<loop>($/) {
        my %parts;
        %parts<setup> := $<e1>.ast if $<e1>;
        %parts<condition> := $<e2>.ast if $<e2>;
        %parts<increment> := $<e3>.ast if $<e3>;
        %parts<body> := $<block>.ast;
        make self.r('Statement', 'Loop').new(|%parts);
    }

    method statement_control:sym<for>($/) {
        make self.r('Statement', 'For').new:
            source => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    ##
    ## Statement prefixes
    ##

    method statement_prefix:sym<END>($/) {
        make self.r('StatementPrefix', 'Phaser', 'End').new($<blorst>.ast);
    }

    method statement_prefix:sym<do>($/) {
        make self.r('StatementPrefix', 'Do').new($<blorst>.ast);
    }

    method statement_prefix:sym<quietly>($/) {
        make self.r('StatementPrefix', 'Quietly').new($<blorst>.ast);
    }

    method blorst($/) {
        make $<block> ?? $<block>.ast !! $<statement>.ast;
    }

    ##
    ## Expression parsing and operators
    ##

    method EXPR($/, $KEY?) {
        my $ast := $/.ast // $<OPER>.ast;
        if $KEY {
            my $key := nqp::lc($KEY);
            if $KEY eq 'INFIX' {
                unless $ast {
                    my $type := $<OPER><O>.made<assoc> eq 'chain'
                        ?? self.r('Infix', 'Chaining')
                        !! self.r('Infix');
                    $ast := $type.new($<infix><sym>);
                }
                make self.r('ApplyInfix').new:
                    infix => $ast,
                    left => $/[0].ast,
                    right => $/[1].ast;
            }
            elsif $KEY eq 'LIST' {
                my @operands;
                for $/.list {
                    @operands.push($_.ast);
                }
                make self.r('ApplyListInfix').new:
                    infix => $ast // self.r('Infix').new($<infix><sym>),
                    operands => @operands;
            }
            elsif $KEY eq 'PREFIX' {
                make self.r('ApplyPrefix').new:
                    prefix => $ast // self.r('Prefix').new($<prefix><sym>),
                    operand => $/[0].ast;
            }
            elsif $KEY eq 'POSTFIX' {
                make self.r('ApplyPostfix').new:
                    postfix => $ast // self.r('Postfix').new($<postfix><sym>),
                    operand => $/[0].ast;
            }
            else {
                nqp::die("EXPR $KEY handling NYI");
            }
        }
        else {
            # Just a term.
            make $ast;
        }
    }

    method postcircumfix:sym<[ ]>($/) {
        make self.r('Postcircumfix', 'ArrayIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<{ }>($/) {
        make self.r('Postcircumfix', 'HashIndex').new($<semilist>.ast);
    }

    method circumfix:sym<ang>($/) { make $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { make $<nibble>.ast; }

    method circumfix:sym<« »>($/) { make $<nibble>.ast; }

    method dotty:sym<.>($/) {
        make $<dottyop>.ast;
    }

    method dottyop($/) {
        if $<methodop> {
            make $<methodop>.ast;
        }
        elsif $<postop> {
            make $<postop>.ast;
        }
        else {
            nqp::die('NYI kind of dottyop');
        }
    }

    method methodop($/) {
        my $args := $<args> ?? $<args>.ast !! self.r('ArgList').new();
        if $<longname> {
            make self.r('Call', 'Method').new(:name($<longname>.ast), :$args);
        }
        else {
            nqp::die('NYI kind of methodop');
        }
    }

    method infixish($/) {
        my $ast;
        if $<infix> {
            $ast := $<infix>.ast;
        }
        else {
            nqp::die('unknown kind of infix');
        }
        if $<infix_postfix_meta_operator> {
            $ast := $<infix_postfix_meta_operator>.ast.new:
                $ast // self.r('Infix').new(~$<infix>);
        }
        make $ast;
    }

    method infix_postfix_meta_operator:sym<=>($/) {
        make self.r('MetaInfix', 'Assign');
    }

    method circumfix:sym<( )>($/) {
        make self.r('Circumfix', 'Parentheses').new($<semilist>.ast);
    }

    method circumfix:sym<[ ]>($/) {
        make self.r('Circumfix', 'ArrayComposer').new($<semilist>.ast);
    }

    method circumfix:sym<{ }>($/) {
        # TODO hash replacement based upon content
        make $<pblock>.ast;
    }

    ##
    ## Terms
    ##

    method term:sym<fatarrow>($/) {
        make self.r('FatArrow').new:
            key => $*LITERALS.intern-str(~$<key>),
            value => $<val>.ast;
    }

    method term:sym<variable>($/) {
        make $<variable>.ast;
    }

    method term:sym<scope_declarator>($/) {
        make $<scope_declarator>.ast;
    }

    method term:sym<routine_declarator>($/) {
        make $<routine_declarator>.ast;
    }

    method term:sym<statement_prefix>($/) {
        make $<statement_prefix>.ast;
    }

    method term:sym<lambda>($/) {
        make $<pblock>.ast;
    }

    method term:sym<value>($/) {
        make $<value>.ast;
    }

    method term:sym<identifier>($/) {
        make self.r('Call', 'Name').new:
            name => self.r('Name').from-identifier(~$<identifier>),
            args => $<args>.ast; 
    }

    method term:sym<name>($/) {
        make self.r('Call', 'Name').new:
            name => $<longname>.ast,
            args => $<args>.ast;
    }

    method variable($/) {
        my str $name := $<sigil> ~ $<desigilname>;
        my $resolution := $*R.resolve-lexical($name);
        if nqp::isconcrete($resolution) {
            make $resolution.generate-lookup();
        }
        else {
            # TODO restore good error
            nqp::die("Undeclared variable $name");
        }
    }

    ##
    ## Declarations
    ##

    method scope_declarator:sym<my>($/)    { make $<scoped>.ast; }
    method scope_declarator:sym<our>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<has>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<HAS>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)  { make $<scoped>.ast; }
    method scope_declarator:sym<state>($/) { make $<scoped>.ast; }

    method scoped($/) {
        make $<DECL>.ast;
    }

    method declarator($/) {
        if $<variable_declarator> {
            make $<variable_declarator>.ast;
        }
        else {
            nqp::die('Unimplemented declarator');
        }
    }

    method initializer:sym<=>($/) {
        make self.r('Initializer', 'Assign').new($<EXPR>.ast);
    }

    method initializer:sym<:=>($/) {
        make self.r('Initializer', 'Bind').new($<EXPR>.ast);
    }

    method variable_declarator($/) {
        my str $scope := $*SCOPE;
        my str $name := $<sigil> ~ $<desigilname>;
        my $initializer := $<initializer>
            ?? $<initializer>.ast
            !! self.r('Initializer');
        my $decl := self.r('VarDeclaration', 'Simple').new(:$scope, :$name, :$initializer);
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            $*R.declare-lexical($decl);
        }
        make $decl;
    }

    method routine_declarator:sym<sub>($/) {
        make $<routine_def>.ast;
    }

    method routine_def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        $routine.replace-body($<blockoid>.ast);
        $routine.calculate-sink();
        make $routine;
    }

    ##
    ## Values
    ##

    method value:sym<quote>($/) {
        make $<quote>.ast;
    }

    method value:sym<number>($/) {
        make $<number>.ast;
    }

    method value:sym<version>($/) {
        make $<version>.ast;
    }

    method number:sym<numish>($/) {
        make $<numish>.ast;
    }

    method numish($/) {
        if $<integer> {
            make self.r('IntLiteral').new($<integer>.ast);
        }
        else {
            nqp::die('unimpl numish');
        }
    }

    method decint($/) {
        make $*LITERALS.intern-int: ~$/, 10, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method hexint($/) {
        make $*LITERALS.intern-int: ~$/, 16, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method octint($/) {
        make $*LITERALS.intern-int: ~$/, 8, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method binint($/) {
        make $*LITERALS.intern-int: ~$/, 2, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method version($/) {
        # We don't make an object for the initial language version line,
        # which occurs before a setting is loaded.
        if $*R {
            my $Version := $*R.resolve-lexical-constant('Version').compile-time-value;
            make self.r('VersionLiteral').new($Version.new(~$<vstr>));
        }
    }

    method quote:sym<apos>($/)  { make $<nibble>.ast; }
    method quote:sym<sapos>($/) { make $<nibble>.ast; }
    method quote:sym<lapos>($/) { make $<nibble>.ast; }
    method quote:sym<hapos>($/) { make $<nibble>.ast; }
    method quote:sym<dblq>($/)  { make $<nibble>.ast; }
    method quote:sym<sdblq>($/) { make $<nibble>.ast; }
    method quote:sym<ldblq>($/) { make $<nibble>.ast; }
    method quote:sym<hdblq>($/) { make $<nibble>.ast; }
    method quote:sym<crnr>($/)  { make $<nibble>.ast; }
    method quote:sym<qq>($/)    { make $<quibble>.ast; }
    method quote:sym<q>($/)     { make $<quibble>.ast; }
    method quote:sym<Q>($/)     { make $<quibble>.ast; }

    method quote:sym</ />($/) {
        make self.r('QuotedRegex').new(body => $<nibble>.ast);
    }

    ##
    ## Signatures
    ##

    method signature($/) {
        my @parameters;
        for $<parameter> {
            my $param := $_.ast;
            # TODO have to twiddle based on sep
            @parameters.push($param);
        }
        make self.r('Signature').new(:@parameters);
    }

    method parameter($/) {
        make $<param_var>.ast;
    }

    method param_var($/) {
        # Work out what kind of thing we're binding into, if any.
        my %args;
        if $<name> {
            my $decl := self.r('ParameterTarget', 'Var').new(~$<declname>);
            $*R.declare-lexical($decl);
            %args<target> := $decl;
        }

        # Build the parameter.
        make self.r('Parameter').new(|%args);
    }

    ##
    ## Argument lists and captures
    ##

    method args($/) {
        if    $<semiarglist> { make $<semiarglist>.ast; }
        elsif $<arglist>     { make $<arglist>.ast; }
        else                 { make self.r('ArgList').new(); }
    }

    method semiarglist($/) {
        if nqp::elems($<arglist>) == 1 {
            make $<arglist>[0].ast;
        }
        else {
            nqp::die('Multiple arg lists NYI')
        }
    }

    method arglist($/) {
        if $<EXPR> {
            my $expr := $<EXPR>.ast;
            if nqp::istype($expr, self.r('ApplyListInfix')) &&
                    nqp::istype($expr.infix, self.r('Infix')) &&
                    $expr.infix.operator eq ',' {
                make self.r('ArgList').from-comma-list($expr);
            }
            else {
                make self.r('ArgList').new($expr);
            }
        }
        else {
            make self.r('ArgList').new();
        }
    }

    ##
    ## Lexer stuff
    ##

    method name($/) {
        nqp::die('multi-part names NYI') if $<morename>;
        make self.r('Name').from-identifier(~$<identifier>);
    }

    method longname($/) {
        # TODO add colonpairs
        make $<name>.ast;
    }

    method deflongname($/) {
        # Set the name on the definition immediately, since it's known at this
        # point onwards.
        my $name := $<name>.ast;
        # TODO add colonpairs
        $*BLOCK.replace-name($name);
    }
}

class Raku::QActions is HLL::Actions {
    proto method r(*@parts) {*}
    multi method r($t) {
        nqp::ifnull(nqp::atkey($ast_root, $t), nqp::die("No such node RakuAST::{$t}"))
    }
    multi method r($t1, $t2) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}"))
    }

    # This overrides NQP during the deprecation period for Unicode 1 names not covered by Alias Names
    method charname-panic($/) { $/.panic("Unrecognized character name [$/]") }

    method charname($/) {
        my $codepoint := $<integer>
                         ?? nqp::chr($<integer>.made)
                         !! nqp::strfromname(~$/);
        $codepoint := self.charname-notfound($/) if $codepoint eq '';
        make $codepoint;
    }

    method charname-notfound($/) {
        my @worry-text := ( "LINE FEED, NEW LINE, END OF LINE, LF, NL or EOL",
                            "FORM FEED or FF",
                            "CARRIAGE RETURN or CR",
                            "NEXT LINE or NEL" );
        my $text := "Deprecated character name [%s] in lookup of Unicode character by name.\n" ~
                    "Unicode 1 names are deprecated.\nPlease use %s";
        if ~$/ eq "LINE FEED (LF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[0]) ) );
            return nqp::strfromname("LINE FEED");
        }
        if ~$/ eq "FORM FEED (FF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[1]) ) );
            return nqp::strfromname("FORM FEED");
        }
        if ~$/ eq "CARRIAGE RETURN (CR)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[2]) ) );
            return nqp::strfromname("CARRIAGE RETURN");
        }
        if ~$/ eq "NEXT LINE (NEL)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[3]) ) );
            return nqp::strfromname("NEXT LINE");
        }
        self.charname-panic($/);
    }

    method nibbler($/) {
        my @segments;
        my $lastlit := '';
        my $StrLiteral := self.r('StrLiteral');

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                my $ast := $_.ast;
                if nqp::isstr($ast) {
                    $lastlit := $lastlit ~ $ast;
                }
                else {
                    if $lastlit ne '' {
                        @segments.push: $StrLiteral.new($*LITERALS.intern-str($lastlit));
                        $lastlit := '';
                    }
                    @segments.push($ast);
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }

        if $lastlit ne '' || !@segments {
            @segments.push: $StrLiteral.new($*LITERALS.intern-str($lastlit));
        }

        my @processors := nqp::can($/, 'postprocessors') ?? $/.postprocessors !! [];
        make self.r('QuotedString').new(:@segments, :@processors);
    }

    method escape:sym<\\>($/) { make $<item>.ast; }
    method backslash:sym<qq>($/) { make $<quote>.ast; }
    method backslash:sym<\\>($/) { make $<text>.Str; }
    method backslash:delim ($/) { make $<text>.Str; }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }

    method backslash:sym<a>($/) { make nqp::chr(7) }
    method backslash:sym<b>($/) { make "\b" }
    method backslash:sym<c>($/) { make $<charspec>.ast }
    method backslash:sym<e>($/) { make "\c[27]" }
    method backslash:sym<f>($/) { make "\c[12]" }
    method backslash:sym<n>($/) {
        # TODO heredoc special handling, $?NL
        make "\n";
    }
    method backslash:sym<o>($/) { make self.ints_to_string( $<octint> ?? $<octint> !! $<octints><octint> ) }
    method backslash:sym<r>($/) {
        # TODO heredoc special handling
        make "\r";
    }
    method backslash:sym<rn>($/) {
        # TODO heredoc special handling
        make "\r\n";
    }
    method backslash:sym<t>($/) {
        # TODO heredoc special handling
        make "\t";
    }
    method backslash:sym<x>($/) { make self.ints_to_string( $<hexint> ?? $<hexint> !! $<hexints><hexint> ) }
    method backslash:sym<0>($/) { make "\c[0]" }

    method escape:sym<$>($/) { make $<EXPR>.ast; }
    method escape:sym<@>($/) { make $<EXPR>.ast; }
    method escape:sym<%>($/) { make $<EXPR>.ast; }
    method escape:sym<&>($/) { make $<EXPR>.ast; }

    method escape:sym<{ }>($/) {
        make $<block>.ast;
    }

    method escape:sym<'>($/) { make self.qwatom($<quote>.ast); }
    method escape:sym<colonpair>($/) { make self.qwatom($<colonpair>.ast); }
    method escape:sym<#>($/) { make ''; }
    method qwatom($ast) { self.r('QuoteWordsAtom').new($ast) }
}

class Raku::RegexActions is HLL::Actions {
    proto method r(*@parts) {*}
    multi method r($t) {
        nqp::ifnull(nqp::atkey($ast_root, $t), nqp::die("No such node RakuAST::{$t}"))
    }
    multi method r($t1, $t2) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}"))
    }
    multi method r($t1, $t2, $t3) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        $res := nqp::atkey($res.WHO, $t3) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}::{$t3}"))
    }
    multi method r($t1, $t2, $t3, $t4) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        $res := nqp::atkey($res.WHO, $t3) unless nqp::isnull($res);
        $res := nqp::atkey($res.WHO, $t4) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}::{$t3}::{$t4}"))
    }

    method nibbler($/) {
        make $<termseq>.ast;
    }

    method termseq($/) {
        make $<termaltseq>.ast;
    }

    method termaltseq($/) {
        if nqp::elems($<termconjseq>) == 1 {
            make $<termconjseq>[0].ast;
        }
        else {
            my @branches;
            for $<termconjseq> {
                @branches.push($_.ast);
            }
            make self.r('Regex', 'SequentialAlternation').new(|@branches);
        }
    }

    method termconjseq($/) {
        if nqp::elems($<termalt>) == 1 {
            make $<termalt>[0].ast;
        }
        else {
            my @branches;
            for $<termalt> {
                @branches.push($_.ast);
            }
            make self.r('Regex', 'SequentialConjunction').new(|@branches);
        }
    }

    method termalt($/) {
        if nqp::elems($<termconj>) == 1 {
            make $<termconj>[0].ast;
        }
        else {
            my @branches;
            for $<termconj> {
                @branches.push($_.ast);
            }
            make self.r('Regex', 'Alternation').new(|@branches);
        }
    }

    method termconj($/) {
        if nqp::elems($<termish>) == 1 {
            make $<termish>[0].ast;
        }
        else {
            my @branches;
            for $<termish> {
                @branches.push($_.ast);
            }
            make self.r('Regex', 'Conjunction').new(|@branches);
        }
    }

    method termish($/) {
        if nqp::elems($<noun>) == 1 {
            make $<noun>[0].ast;
        }
        else {
            my @terms;
            for $<noun> {
                @terms.push($_.ast);
            }
            make self.r('Regex', 'Sequence').new(|@terms);
        }
    }

    method quantified_atom($/) {
        my $atom := $<atom>.ast;
        if $<quantifier> {
            my $quantifier := $<quantifier>.ast;
            if $<separator> {
                my $separator := $<separator>.ast;
                my $trailing-separator := $<separator><septype> eq '%%';
                make self.r('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier,
                    :$separator, :$trailing-separator);
            }
            else {
                make self.r('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier);
            }
        }
        else {
            if $<separator> {
                $/.panic("'" ~ $<separator><septype> ~
                    "' may only be used immediately following a quantifier")
            }
            make $atom;
        }
    }

    method atom($/) {
        if $<metachar> {
            make $<metachar>.ast;
        }
        else {
            make self.r('Regex', 'Literal').new(~$/);
        }
    }

    method quantifier:sym<*>($/) {
        make self.r('Regex', 'Quantifier', 'ZeroOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<+>($/) {
        make self.r('Regex', 'Quantifier', 'OneOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<?>($/) {
        make self.r('Regex', 'Quantifier', 'ZeroOrOne').new(backtrack => $<backmod>.ast);
    }

    method backmod($/) {
        my str $backmod := ~$/;
        if $backmod eq ':' {
            self.r('Regex', 'Backtrack', 'Ratchet')
        }
        elsif $backmod eq ':?' || $backmod eq '?' {
            self.r('Regex', 'Backtrack', 'Frugal')
        }
        elsif $backmod eq ':!' || $backmod eq '!' {
            self.r('Regex', 'Backtrack', 'Greedy')
        }
        else {
            self.r('Regex', 'Backtrack')
        }
    }

    method separator($/) {
        make $<quantified_atom>.ast;
    }

    method metachar:sym<[ ]>($/) {
        make self.r('Regex', 'Group').new($<nibbler>.ast);
    }

    method metachar:sym<( )>($/) {
        make self.r('Regex', 'CapturingGroup').new($<nibbler>.ast);
    }

    method metachar:sym<.>($/) {
        make self.r('Regex', 'CharClass', 'Any').new;
    }

    method metachar:sym<^>($/) {
        make self.r('Regex', 'Anchor', 'BeginningOfString').new;
    }

    method metachar:sym<^^>($/) {
        make self.r('Regex', 'Anchor', 'BeginningOfLine').new;
    }

    method metachar:sym<$>($/) {
        make self.r('Regex', 'Anchor', 'EndOfString').new;
    }

    method metachar:sym<$$>($/) {
        make self.r('Regex', 'Anchor', 'EndOfLine').new;
    }

    method metachar:sym<lwb>($/) {
        make self.r('Regex', 'Anchor', 'LeftWordBoundary').new;
    }

    method metachar:sym<rwb>($/) {
        make self.r('Regex', 'Anchor', 'RightWordBoundary').new;
    }

    method metachar:sym<from>($/) {
        make self.r('Regex', 'MatchFrom').new;
    }

    method metachar:sym<to>($/) {
        make self.r('Regex', 'MatchTo').new;
    }

    method metachar:sym<bs>($/) {
        make $<backslash>.ast;
    }

    method metachar:sym<assert>($/) {
        make $<assertion>.ast;
    }

    method metachar:sym<qw>($/) {
        make self.r('Regex', 'Quote').new($<nibble>.ast);
    }

    method metachar:sym<'>($/) {
        make self.r('Regex', 'Quote').new($<quote>.ast);
    }

    method backslash:sym<s>($/) {
        my constant NAME := nqp::hash('d', 'Digit', 'n', 'Newline', 's', 'Space', 'w', 'Word');
        make self.r('Regex', 'CharClass', NAME{nqp::lc(~$<sym>)}).new(negated => $<sym> le 'Z');
    }

    method backslash:sym<misc>($/) {
        make self.r('Regex', 'Literal').new(~$/);
    }

    method assertion:sym<?>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            make self.r('Regex', 'Assertion', 'Lookahead').new(:$assertion);
        }
        else {
            make self.r('Regex', 'Assertion', 'Pass').new();
        }
    }

    method assertion:sym<!>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            make self.r('Regex', 'Assertion', 'Lookahead').new(:$assertion, :negated);
        }
        else {
            make self.r('Regex', 'Assertion', 'Fail').new();
        }
    }

    method assertion:sym<method>($/) {
        my $ast := $<assertion>.ast;
        if nqp::can($ast, 'set-capturing') {
            $ast.set-capturing(0);
        }
        make $ast;
    }

    method assertion:sym<name>($/) {
        my $name := ~$<longname>;
        my $qast;
        if $<assertion> {
            my str $name := ~$<longname>;
            my $assertion := $<assertion>.ast;
            make self.r('Regex', 'Assertion', 'Alias').new(:$name, :$assertion);
        }
        else {
            my $name := $<longname>.ast;
            if $<arglist> {
                my $args := $<arglist>.ast;
                make self.r('Regex', 'Assertion', 'Named', 'Args').new(
                    :$name, :capturing, :$args);
            }
            elsif $<nibbler> {
                my $regex-arg := $<nibbler>.ast;
                make self.r('Regex', 'Assertion', 'Named', 'RegexArg').new(
                    :$name, :capturing, :$regex-arg);
            }
            else {
                make self.r('Regex', 'Assertion', 'Named').new(:$name, :capturing);
            }
        }
    }
}
