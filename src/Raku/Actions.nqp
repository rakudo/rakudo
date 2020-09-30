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
        my $version := nqp::substr(nqp::getcomp('Raku').language_version, 2);
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting-name := $loader.transform_setting_name("$name.$version");

        # Set up the resolver.
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
        if nqp::isconcrete($outer_ctx) {
            my $global := %*COMPILING<%?OPTIONS><global>;
            $*R := $resolver_type.from-context(:context($outer_ctx), :$global);
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
            $*R.set-global($*CU.generated-global);
        }

        # Set up the literals builder, so we can produce and intern literal
        # values.
        $*LITERALS := self.r('LiteralBuilder').new(:resolver($*R));
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
            make self.r('Statement', 'Expression').new(expression => $<EXPR>.ast);
        }
        elsif $<statement_control> {
            make $<statement_control>.ast;
        }
        else {
            make self.r('Statement', 'Empty').new;
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

    method statement_control:sym<given>($/) {
        make self.r('Statement', 'Given').new:
            source => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<when>($/) {
        make self.r('Statement', 'When').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<default>($/) {
        make self.r('Statement', 'Default').new(body => $<block>.ast);
    }

    method statement_control:sym<CATCH>($/) {
        make self.r('Statement', 'Catch').new(body => $<block>.ast);
    }

    method statement_control:sym<CONTROL>($/) {
        make self.r('Statement', 'Control').new(body => $<block>.ast);
    }

    method statement_control:sym<use>($/) {
        my $ast := $<arglist><EXPR>
            ?? self.r('Statement', 'Use').new(
                   module-name => $<module_name>.ast,
                   argument => $<arglist><EXPR>.ast
               )
            !! self.r('Statement', 'Use').new(module-name => $<module_name>.ast);
        $ast.ensure-begin-performed($*R);
        make $ast;
    }

    ##
    ## Statement prefixes
    ##

    method statement_prefix:sym<BEGIN>($/) {
        my $ast := self.r('StatementPrefix', 'Phaser', 'Begin').new($<blorst>.ast);
        $ast.ensure-begin-performed($*R);
        make $ast;
    }

    method statement_prefix:sym<END>($/) {
        make self.r('StatementPrefix', 'Phaser', 'End').new($<blorst>.ast);
    }

    method statement_prefix:sym<race>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('race');
            make $blorst;
        }
        else {
            make self.r('StatementPrefix', 'Race').new($blorst);
        }
    }

    method statement_prefix:sym<hyper>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('hyper');
            make $blorst;
        }
        else {
            make self.r('StatementPrefix', 'Hyper').new($blorst);
        }
    }

    method statement_prefix:sym<lazy>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('lazy');
            make $blorst;
        }
        else {
            make self.r('StatementPrefix', 'Lazy').new($blorst);
        }
    }

    method statement_prefix:sym<eager>($/) {
        make self.r('StatementPrefix', 'Eager').new($<blorst>.ast);
    }

    method statement_prefix:sym<try>($/) {
        make self.r('StatementPrefix', 'Try').new($<blorst>.ast);
    }

    method statement_prefix:sym<do>($/) {
        make self.r('StatementPrefix', 'Do').new($<blorst>.ast);
    }

    method statement_prefix:sym<quietly>($/) {
        make self.r('StatementPrefix', 'Quietly').new($<blorst>.ast);
    }

    method statement_prefix:sym<gather>($/) {
        make self.r('StatementPrefix', 'Gather').new($<blorst>.ast);
    }

    method statement_prefix:sym<start>($/) {
        make self.r('StatementPrefix', 'Start').new($<blorst>.ast);
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
                my $sym := $<infix><sym>;
                if $sym && $sym eq '??' {
                    make self.r('Ternary').new:
                        condition => $/[0].ast,
                        then => $/[1].ast,
                        else => $/[2].ast;
                }
                elsif $ast && nqp::istype($ast, self.r('DottyInfixish')) {
                    make self.r('ApplyDottyInfix').new:
                        infix => $ast,
                        left => $/[0].ast,
                        right => $/[1].ast;
                }
                else {
                    unless $ast {
                        my $type := $<OPER><O>.made<assoc> eq 'chain'
                            ?? self.r('Infix', 'Chaining')
                            !! self.r('Infix');
                        $ast := $type.new($sym);
                    }
                    make self.r('ApplyInfix').new:
                        infix => $ast,
                        left => $/[0].ast,
                        right => $/[1].ast;
                }
            }
            elsif $KEY eq 'LIST' {
                my @operands;
                for $/.list {
                    my $ast := $_.ast;
                    @operands.push($ast) if nqp::isconcrete($ast);
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

    method postop($/) {
        make $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
    }

    method postcircumfix:sym<[ ]>($/) {
        make self.r('Postcircumfix', 'ArrayIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<{ }>($/) {
        make self.r('Postcircumfix', 'HashIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<ang>($/) {
        make self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym«<< >>»($/) {
        make self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym<« »>($/) {
        make self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym<( )>($/) {
        make self.r('Call', 'Term').new(args => $<arglist>.ast);
    }

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
        make $<pblock>.ast.block-or-hash;
    }

    method circumfix:sym<ang>($/) { make $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { make $<nibble>.ast; }

    method circumfix:sym<« »>($/) { make $<nibble>.ast; }

    method infix:sym<.>($/) {
        make self.r('DottyInfix', 'Call').new;
    }

    method infix:sym<.=>($/) {
        make self.r('DottyInfix', 'CallAssign').new;
    }

    ##
    ## Terms
    ##

    method term:sym<self>($/) {
        make self.r('Term', 'Self').new();
    }

    method term:sym<now>($/) {
        make self.r('Term', 'Named').new('now');
    }

    method term:sym<time>($/) {
        make self.r('Term', 'Named').new('time');
    }

    method term:sym<empty_set>($/) {
        make self.r('Term', 'EmptySet').new();
    }

    method term:sym<rand>($/) {
        make self.r('Term', 'Rand').new();
    }

    method term:sym<fatarrow>($/) {
        make self.r('FatArrow').new:
            key => $*LITERALS.intern-str(~$<key>),
            value => $<val>.ast;
    }

    method term:sym<colonpair>($/) {
        make $<colonpair>.ast;
    }

    method term:sym<variable>($/) {
        make $<variable>.ast;
    }

    method term:sym<package_declarator>($/) {
        make $<package_declarator>.ast;
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

    method term:sym<*>($/) {
        make self.r('Term', 'Whatever').new;
    }

    method term:sym<**>($/) {
        make self.r('Term', 'HyperWhatever').new;
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
        if $<args> {
            make self.r('Call', 'Name').new:
                name => $<longname>.ast,
                args => $<args>.ast;
        }
        else {
            if $*is-type {
                make self.r('Type', 'Simple').new($<longname>.ast);
            }
            else {
                make self.r('Term', 'Name').new($<longname>.ast);
            }
        }
    }

    method term:sym<dotty>($/) {
        make self.r('Term', 'TopicCall').new($<dotty>.ast);
    }

    method term:sym<capture>($/) {
        make self.r('Term', 'Capture').new($<args>.ast);
    }

    method colonpair($/) {
        my $key-str := $*key;
        if $key-str {
            my $key := $*LITERALS.intern-str($key-str);
            if $<num> {
                my $value := self.r('IntLiteral').new($*LITERALS.intern-int(~$<num>, 10));
                make self.r('ColonPair', 'Number').new(:$key, :$value);
            }
            elsif $<coloncircumfix> {
                my $value := $<coloncircumfix>.ast;
                make self.r('ColonPair', 'Value').new(:$key, :$value);
            }
            elsif $<var> {
                my $value := $<var>.ast; 
                make self.r('ColonPair', 'Variable').new(:$key, :$value);
            }
            elsif $<neg> {
                make self.r('ColonPair', 'False').new(:$key);
            }
            else {
                make self.r('ColonPair', 'True').new(:$key);
            }
        }
        else {
            nqp::die('non-key colonpair compilation NYI');
        }
    }

    method coloncircumfix($/) {
        make $<circumfix>
            ?? $<circumfix>.ast
            !! self.r('Term', 'Name').new(self.r('Name').from-identifier('Nil'));
    }

    method colonpair_variable($/) {
        if $<capvar> {
            make self.r('Var', 'NamedCapture').new($*LITERALS.intern-str(~$<desigilname>));
        }
        else {
            make self.r('Var', 'Lexical').new(~$/);
        }
    }

    method variable($/) {
        if $<index> {
            make self.r('Var', 'PositionalCapture').new($*LITERALS.intern-int(~$<index>, 10));
        }
        elsif $<postcircumfix> {
            make self.r('Var', 'NamedCapture').new($<postcircumfix>.ast.index);
        }
        else {
            my str $twigil := $<twigil> ?? ~$<twigil> !! '';
            my str $name := $<sigil> ~ $twigil ~ $<desigilname>;
            if $twigil eq '' {
                my $resolution := $*R.resolve-lexical($name);
                if nqp::isconcrete($resolution) {
                    make $resolution.generate-lookup();
                }
                elsif $<sigil> eq '&' {
                    # Can be resolved late-bound.
                    make self.r('Var', 'Lexical').new($name);
                }
                else {
                    # TODO restore good error
                    nqp::die("Undeclared variable $name");
                }
            }
            elsif $twigil eq '*' {
                make self.r('Var', 'Dynamic').new($name);
            }
            elsif $twigil eq '!' {
                make self.r('Var', 'Attribute').new($name);
            }
            elsif $twigil eq '?' {
                if $name eq '$?FILE' {
                    my str $file := self.current_file();
                    make self.r('Var', 'Compiler', 'File').new($*LITERALS.intern-str($file));
                }
                elsif $name eq '$?LINE' {
                    my int $line := self.current_line($/);
                    make self.r('Var', 'Compiler', 'Line').new($*LITERALS.intern-int($line, 10));
                }
                else {
                    make self.r('Var', 'Compiler', 'Lookup').new($name);
                }
            }
            else {
                nqp::die("Lookup with twigil '$twigil' NYI");
            }
        }
    }

    method current_file() {
        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isnull($file) {
            $file := '<unknown file>';
        }
        elsif !nqp::eqat($file,'/',0) && !nqp::eqat($file,'-',0) && !nqp::eqat($file,':',1) {
            $file := nqp::cwd ~ '/' ~ $file;
        }
        $file;
    }

    method current_line($/) {
        HLL::Compiler.lineof($/.orig,$/.from,:cache(1));
    }

    ##
    ## Declarations
    ##

    method package_declarator:sym<package>($/) { make $<package_def>.ast; }
    method package_declarator:sym<module>($/)  { make $<package_def>.ast; }
    method package_declarator:sym<class>($/)   { make $<package_def>.ast; }
    method package_declarator:sym<grammar>($/) { make $<package_def>.ast; }
    method package_declarator:sym<role>($/)    { make $<package_def>.ast; }
    method package_declarator:sym<knowhow>($/) { make $<package_def>.ast; }
    method package_declarator:sym<native>($/)  { make $<package_def>.ast; }

    method package_def($/) {
        my $package := $*PACKAGE;
        $package.replace-body($<block>.ast);
        make $package;
    }

    method stub-package($/) {
        # Resolve the meta-object.
        my $package-declarator := $*PKGDECL;
        my $how;
        if $/.know_how($package-declarator) {
            $how := $/.how($package-declarator);
        }
        else {
            $/.panic("Cannot resolve meta-object for $package-declarator")
        }

        # Stub the package AST node.
        my str $scope := $*SCOPE // 'our';
        my $name-match := $*PACKAGE-NAME;
        my $name := $name-match ?? $name-match.ast !! self.r('Name');
        $*PACKAGE := self.r('Package').new: :$package-declarator, :$how, :$name, :$scope;
    }

    method enter-package-scope($/) {
        # Perform BEGIN-time effects (declaring the package, applying traits,
        # etc.)
        $*PACKAGE.ensure-begin-performed($*R);

        # Let the resovler know which package we're in.
        $*R.push-package($*PACKAGE);
    }

    method leave-package-scope($/) {
        $*R.pop-package();
    }

    method scope_declarator:sym<my>($/)    { make $<scoped>.ast; }
    method scope_declarator:sym<our>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<has>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<HAS>($/)   { make $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)  { make $<scoped>.ast; }
    method scope_declarator:sym<state>($/) { make $<scoped>.ast; }

    method scoped($/) {
        make $<DECL>.ast;
    }

    method multi_declarator:sym<null>($/) {
        make $<declarator>.ast;
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
        my $type := $*OFTYPE ?? $*OFTYPE.ast !! self.r('Type');
        my str $name := $<sigil> ~ ($<twigil> || '') ~ $<desigilname>;
        my $initializer := $<initializer>
            ?? $<initializer>.ast
            !! self.r('Initializer');
        my $decl := self.r('VarDeclaration', 'Simple').new:
            :$scope, :$type, :$name, :$initializer;
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            $*R.declare-lexical($decl);
        }
        make $decl;
    }

    method routine_declarator:sym<sub>($/) {
        make $<routine_def>.ast;
    }
    method routine_declarator:sym<method>($/) {
        make $<method_def>.ast;
    }
    method routine_declarator:sym<submethod>($/) {
        make $<method_def>.ast;
    }

    method routine_def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        $routine.replace-body($<blockoid>.ast);
        $routine.calculate-sink();
        $routine.ensure-begin-performed($*R);
        make $routine;
    }

    method method_def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        $routine.replace-body($<blockoid>.ast);
        $routine.calculate-sink();
        make $routine;
    }

    method trait($/) {
        my $trait := $<trait_mod>.ast;
        if $*TARGET {
            # Already have the target to apply it to.
            $*TARGET.add-trait($trait);
        }
        else {
            # Will be added to a target later, so attach to the match.
            make $trait;
        }
    }

    method trait_mod:sym<hides>($/) {
        make self.r('Trait', 'Hides').new($<typename>.ast);
    }

    method trait_mod:sym<does>($/) {
        make self.r('Trait', 'Does').new($<typename>.ast);
    }

    method trait_mod:sym<of>($/) {
        make self.r('Trait', 'Of').new($<typename>.ast);
    }

    method trait_mod:sym<returns>($/) {
        make self.r('Trait', 'Returns').new($<typename>.ast);
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
        elsif $<dec_number> {
            make $<dec_number>.ast;
        }
        elsif $<rat_number> {
            make $<rat_number>.ast;
        }
        elsif $<unum> {
            my $code := nqp::ord($/.Str);
            my int $nu := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Numerator"));
            my int $de := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Denominator"));
            if !$de || $de == 1 {
                make self.r('IntLiteral').new($*LITERALS.intern-int($nu, 10));
            }
            else {
                make self.r('RatLiteral').new($*LITERALS.intern-rat(
                    $*LITERALS.intern-int($nu, 10),
                    $*LITERALS.intern-int($de, 10)
                ));
            }
        }
        elsif $<uinf> {
            make self.r('NumLiteral').new($*LITERALS.intern-num('Inf'));
        }
        else {
            make self.r('NumLiteral').new($*LITERALS.intern-num(~$/));
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

    method signed-integer($/) {
        my $integer := $<integer>.ast;
        make $<sign> eq '-' || $<sign> eq '−'
            ?? nqp::neg_I($integer, $integer.WHAT)
            !! $integer;
    }

    method dec_number($/) {
        if $<escale> { # wants a Num
            make self.r('NumLiteral').new($*LITERALS.intern-num(~$/));
        }
        else { # wants a Rat
            make self.r('RatLiteral').new($*LITERALS.intern-rat(
                $<int> ?? $<int>.ast !! NQPMu,
                ~$<frac>));
        }
    }

    method rat_number($/) {
        make $<bare_rat_number>.ast;
    }

    method bare_rat_number($/) {
        make self.r('RatLiteral').new($*LITERALS.intern-rat($<nu>.ast, $<de>.ast));
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
    ## Types
    ##

    method typename($/) {
        my $base-name := $<longname>
            ?? $<longname>.ast
            !! self.r('Name').from-identifier(~$<identifier>);
        make self.r('Type', 'Simple').new($base-name);
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
        my $parameter := $<param_var>   ?? $<param_var>.ast   !!
                         $<named_param> ?? $<named_param>.ast !!
                         $<param_term>  ?? $<param_term>.ast  !!
                         self.r('Parameter').new;
        if $<type_constraint> {
            $parameter.set-type($<type_constraint>.ast);
        }
        if $<quant> {
            my str $q := ~$<quant>;
            $parameter.set-optional(1) if $q eq '?';
            $parameter.set-optional(0) if $q eq '!';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Flattened'))
                if $q eq '*';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Unflattened'))
                if $q eq '**';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'SingleArgument'))
                if $q eq '+';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Capture'))
                if $q eq '|';
        }
        make $parameter;
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

    method param_term($/) {
        if $<defterm> {
            # Create sigilless target to bind into
            my $decl := self.r('ParameterTarget', 'Term').new($<defterm>.ast);
            $*R.declare-lexical($decl);
            make self.r('Parameter').new(target => $decl);
        }
        else {
            # Anonymous
            make self.r('Parameter').new();
        }
    }

    method named_param($/) {
        my $parameter;
        if $<name> {
            # Explicitly specified name to attach.
            if $<named_param> {
                $parameter := $<named_param>.ast;
            }
            else {
                $parameter := $<param_var>.ast;
                $parameter.set-optional(1);
            }
            $parameter.add-name(~$<name>);
        }
        else {
            # Name comes from the parameter variable.
            $parameter := $<param_var>.ast;
            $parameter.set-optional(1);
            my $name-match := $<param_var><name>;
            $parameter.add-name($name-match ?? ~$name-match !! '');
        }
        make $parameter;
    }

    method type_constraint($/) {
        make $<typename>.ast;
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
        if $<morename> {
            my @parts;
            if $<identifier> {
                @parts.push(self.r('Name', 'Part', 'Simple').new(~$<identifier>));
            }
            for $<morename> {
                @parts.push($_.ast);
            }
            make self.r('Name').new(|@parts);
        }
        else {
            make self.r('Name').from-identifier(~$<identifier>);
        }
    }

    method morename($/) {
        if $<identifier> {
            make self.r('Name', 'Part', 'Simple').new(~$<identifier>);
        }
        else {
            nqp::die('Complex name parts NYI');
        }
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

        # Register it with the resolver.
        my $scope := $*SCOPE || $*DEFAULT-SCOPE;
        if $scope eq 'my' {
            $*R.declare-lexical-in-outer($*BLOCK);
        }
        else {
            $*R.declare-lexical($*BLOCK);
        }
    }

    method defterm($/) {
        make self.r('Name').from-identifier(~$/);
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
