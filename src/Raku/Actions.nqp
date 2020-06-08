use NQPP6QRegex;
use NQPP5QRegex;

class Raku::Actions is HLL::Actions {
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
        my $cu := $*CU;
        $cu.replace-statement-list($<statementlist>.ast);
        $cu.resolve-all($*R);
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
        else {
            nqp::die('unimpl statemnet type');
        }
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
