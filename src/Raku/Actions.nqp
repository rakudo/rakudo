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

class Raku::Actions is HLL::Actions {
    proto method r(*@parts) {*}
    multi method r($t) {
        nqp::ifnull(nqp::atkey($ast_root, $t), nqp::die("No such node RakuAST::{$t}"))
    }
    multi method r($t1, $t2) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}"))
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

        # Create a compilation unit.
        my $file := nqp::getlexdyn('$?FILES');
        my $comp-unit-name := nqp::sha1($file ~ (
            nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>)
                ?? $/.target() ~ SerializationContextId.next-id()
                !! $/.target()));
        $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name);

        # Set up the resolver.
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
        if nqp::isconcrete($outer_ctx) {
            $*R := $resolver_type.from-context(:context($outer_ctx));
        }
        else {
            $*R := $resolver_type.from-setting(:$setting-name);
        }

        # Set up the literals builder, so we can produce and intern literal
        # values.
        $*LITERALS := self.r('LiteralBuilder').new();
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

    method statement_control:sym<unless>($/) {
        make self.r('Statement', 'Unless').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    ##
    ## Expression parsing and operators
    ##

    method EXPR($/, $KEY?) {
        if $KEY {
            my $key := nqp::lc($KEY);
            if $KEY eq 'INFIX' {
                make self.r('ApplyInfix').new:
                    infix => $<infix>.ast // self.r('Infix').new($<infix><sym>),
                    left => $/[0].ast,
                    right => $/[1].ast;
            }
            elsif $KEY eq 'LIST' {
                my @operands;
                for $/.list {
                    @operands.push($_.ast);
                }
                make self.r('ApplyListInfix').new:
                    infix => $<infix>.ast // self.r('Infix').new($<infix><sym>),
                    operands => @operands;
            }
            elsif $KEY eq 'PREFIX' {
                make self.r('ApplyPrefix').new:
                    prefix => $<prefix>.ast // self.r('Prefix').new($<prefix><sym>),
                    operand => $/[0].ast;
            }
            elsif $KEY eq 'POSTFIX' {
                make self.r('ApplyPostfix').new:
                    postfix => $<postfix>.ast // self.r('Postfix').new($<postfix><sym>),
                    operand => $/[0].ast;
            }
            else {
                nqp::die("EXPR $KEY handling NYI");
            }
        }
        else {
            # Just a term.
            make $/.ast;
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
        make $ast;
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

    method scope_declarator:sym<my>($/) { make $<scoped>.ast; }

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
        my str $name := $<sigil> ~ $<desigilname>;
        my $initializer := $<initializer>
            ?? $<initializer>.ast
            !! self.r('Initializer');
        my $decl := self.r('Declaration', 'Var').new(:$name, :$initializer);
        $*R.declare-lexical($decl);
        make $decl;
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

    ##
    ## Signatures
    ##

    method signature($/) {
        make self.r('Signature').new;
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

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                if nqp::isstr($_.ast) {
                    $lastlit := $lastlit ~ $_.ast;
                }
                else {
                    nqp::die('complex quoted strings NYI');
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }

        if $lastlit ne '' || !@segments {
            @segments.push: self.r('StrLiteral').new($*LITERALS.intern-str($lastlit));
        }

        make self.r('QuotedString').new(|@segments);
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
}
