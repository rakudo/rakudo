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
    # Some AST nodes need symbol resolution or attachment of position information
    # as we go. This factors out that process and attaches the AST to the match
    # object.
    method attach($/, $node) {
        if nqp::istype($node, self.r('ImplicitLookups')) {
            $node.resolve-implicit-lookups-with($*R);
        }
        if nqp::istype($node, self.r('Attaching')) {
            $node.attach($*R);
        }
        make $node;
    }

    method quibble($/) {
        self.attach: $/, $<nibble>.ast;
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
        my %options := %*COMPILING<%?OPTIONS>;
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %options<outer_ctx>;
        my $precompilation-mode := %options<precomp>;
        if nqp::isconcrete($outer_ctx) {
            my $global := %options<global>;
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

        my $package-how := $*LANG.how('package');
        my $export-package := $package-how.new_type(name => 'EXPORT');
        $export-package.HOW.compose($export-package);

        $*R.set-export-package($export-package);
        $*EXPORT := $export-package;

        # Create a compilation unit.
        my $file := nqp::getlexdyn('$?FILES');
        if nqp::isconcrete($outer_ctx) {
            # It's an EVAL. We'll take our GLOBAL, $?PACKAGE, etc. from that.
            my $comp-unit-name := nqp::sha1($file ~ $/.target() ~ SerializationContextId.next-id());
            $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name, :eval, :$precompilation-mode);
        }
        else {
            # Top-level compilation. Create a GLOBAL using the correct package meta-object.
            my $comp-unit-name := nqp::sha1($file ~ $/.target());
            $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name,
                :global-package-how($package-how), :$precompilation-mode,
                :$export-package);
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
        my $compilation-exception := $*R.produce-compilation-exception;
        if nqp::isconcrete($compilation-exception) {
            if $*R.has-compilation-errors {
                # Really has errors, so report them.
                $compilation-exception.throw;
            }
            else {
                # Only potential difficulties, just just print them.
                stderr().print($compilation-exception.gist);
            }
        }

        self.attach: $/, $cu;
    }

    ##
    ## Statements
    ##

    method statementlist($/) {
        my $list := self.r('StatementList').new();
        for $<statement> {
            $list.push($_.ast);
        }
        self.attach: $/, $list;
    }

    method semilist($/) {
        my $list := self.r('SemiList').new();
        for $<statement> {
            $list.push($_.ast);
        }
        self.attach: $/, $list;
    }

    method sequence($/) {
        my $sequence := self.r('StatementSequence').new();
        for $<statement> {
            $sequence.push($_.ast);
        }
        self.attach: $/, $sequence;
    }

    method statement($/) {
        if $<EXPR> {
            my $statement := self.r('Statement', 'Expression').new(expression => $<EXPR>.ast);
            if $<statement_mod_cond> {
                $statement.replace-condition-modifier($<statement_mod_cond>.ast);
            }
            if $<statement_mod_loop> {
                $statement.replace-loop-modifier($<statement_mod_loop>.ast);
            }
            self.attach: $/, $statement;
        }
        elsif $<statement_control> {
            self.attach: $/, $<statement_control>.ast;
        }
        elsif $<label> {
            my $statement := $<statement>.ast;
            $statement.add-label($<label>.ast);
            make $statement;
        }
        else {
            self.attach: $/, self.r('Statement', 'Empty').new;
        }
    }

    method label($/) {
        my $label := self.r('Label').new(~$<identifier>);
        $*R.declare-lexical($label);
        self.attach: $/, $label;
    }

    method pblock($/) {
        my $block := $*BLOCK;
        if $<signature> {
            $block.replace-signature($<signature>.ast);
        }
        $block.replace-body($<blockoid>.ast);
        $block.ensure-begin-performed($*R);
        self.attach: $/, $block;
    }

    method block($/) {
        my $block := $*BLOCK;
        $block.replace-body($<blockoid>.ast);
        $block.ensure-begin-performed($*R);
        self.attach: $/, $block;
    }

    method blockoid($/) {
        self.attach: $/, self.r('Blockoid').new($<statementlist>.ast);
    }

    method onlystar($/) {
        self.attach: $/, self.r('OnlyStar').new;
    }

    method enter-block-scope($/) {
        my $block := $*MULTINESS
            ?? self.r($*SCOPE-KIND).new(:multiness($*MULTINESS))
            !! self.r($*SCOPE-KIND).new;
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
        self.attach: $/, self.r('Statement', $<sym>[0] eq 'with' ?? 'With' !! 'If').new:
            :$condition, :$then, :@elsifs, :$else;
    }

    method statement_control:sym<unless>($/) {
        self.attach: $/, self.r('Statement', 'Unless').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<without>($/) {
        self.attach: $/, self.r('Statement', 'Without').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<while>($/) {
        self.attach: $/, self.r('Statement', 'Loop', $<sym> eq 'while' ?? 'While' !! 'Until').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<repeat>($/) {
        self.attach: $/, self.r('Statement', 'Loop', $<wu> eq 'while' ?? 'RepeatWhile' !! 'RepeatUntil').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<loop>($/) {
        my %parts;
        %parts<setup> := $<e1>.ast if $<e1>;
        %parts<condition> := $<e2>.ast if $<e2>;
        %parts<increment> := $<e3>.ast if $<e3>;
        %parts<body> := $<block>.ast;
        self.attach: $/, self.r('Statement', 'Loop').new(|%parts);
    }

    method statement_control:sym<for>($/) {
        self.attach: $/, self.r('Statement', 'For').new:
            source => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<given>($/) {
        self.attach: $/, self.r('Statement', 'Given').new:
            source => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<when>($/) {
        self.attach: $/, self.r('Statement', 'When').new:
            condition => $<EXPR>.ast,
            body => $<pblock>.ast;
    }

    method statement_control:sym<default>($/) {
        self.attach: $/, self.r('Statement', 'Default').new(body => $<block>.ast);
    }

    method statement_control:sym<CATCH>($/) {
        self.attach: $/, self.r('Statement', 'Catch').new(body => $<block>.ast);
    }

    method statement_control:sym<CONTROL>($/) {
        self.attach: $/, self.r('Statement', 'Control').new(body => $<block>.ast);
    }

    method statement_control:sym<use>($/) {
        my $ast := $<arglist><EXPR>
            ?? self.r('Statement', 'Use').new(
                   module-name => $<module_name>.ast,
                   argument => $<arglist><EXPR>.ast
               )
            !! self.r('Statement', 'Use').new(module-name => $<module_name>.ast);
        $ast.ensure-begin-performed($*R);
        self.attach: $/, $ast;
    }

    ##
    ## Statement modifiers
    ##

    method modifier_expr($/) {
        self.attach: $/, $<EXPR>.ast;
    }

    method statement_mod_cond:sym<if>($/) {
        self.attach: $/, self.r('StatementModifier', 'If').new($<modifier_expr>.ast);
    }
    method statement_mod_cond:sym<unless>($/) {
        self.attach: $/, self.r('StatementModifier', 'Unless').new($<modifier_expr>.ast);
    }
    method statement_mod_cond:sym<when>($/) {
        self.attach: $/, self.r('StatementModifier', 'When').new($<modifier_expr>.ast);
    }
    method statement_mod_cond:sym<with>($/) {
        self.attach: $/, self.r('StatementModifier', 'With').new($<modifier_expr>.ast);
    }
    method statement_mod_cond:sym<without>($/) {
        self.attach: $/, self.r('StatementModifier', 'Without').new($<modifier_expr>.ast);
    }

    method statement_mod_loop:sym<while>($/) {
        self.attach: $/, self.r('StatementModifier', 'While').new($<modifier_expr>.ast);
    }
    method statement_mod_loop:sym<until>($/) {
        self.attach: $/, self.r('StatementModifier', 'Until').new($<modifier_expr>.ast);
    }
    method statement_mod_loop:sym<given>($/) {
        self.attach: $/, self.r('StatementModifier', 'Given').new($<modifier_expr>.ast);
    }
    method statement_mod_loop:sym<for>($/) {
        self.attach: $/, self.r('StatementModifier', 'For').new($<modifier_expr>.ast);
    }

    ##
    ## Statement prefixes
    ##

    method statement_prefix:sym<BEGIN>($/) {
        my $ast := self.r('StatementPrefix', 'Phaser', 'Begin').new($<blorst>.ast);
        $ast.ensure-begin-performed($*R);
        self.attach: $/, $ast;
    }

    method statement_prefix:sym<END>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Phaser', 'End').new($<blorst>.ast);
    }

    method statement_prefix:sym<race>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('race');
            self.attach: $/, $blorst;
        }
        else {
            self.attach: $/, self.r('StatementPrefix', 'Race').new($blorst);
        }
    }

    method statement_prefix:sym<hyper>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('hyper');
            self.attach: $/, $blorst;
        }
        else {
            self.attach: $/, self.r('StatementPrefix', 'Hyper').new($blorst);
        }
    }

    method statement_prefix:sym<lazy>($/) {
        my $blorst := $<blorst>.ast;
        if nqp::istype($blorst, self.r('Statement', 'For')) {
            $blorst.replace-mode('lazy');
            self.attach: $/, $blorst;
        }
        else {
            self.attach: $/, self.r('StatementPrefix', 'Lazy').new($blorst);
        }
    }

    method statement_prefix:sym<eager>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Eager').new($<blorst>.ast);
    }

    method statement_prefix:sym<try>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Try').new($<blorst>.ast);
    }

    method statement_prefix:sym<do>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Do').new($<blorst>.ast);
    }

    method statement_prefix:sym<quietly>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Quietly').new($<blorst>.ast);
    }

    method statement_prefix:sym<gather>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Gather').new($<blorst>.ast);
    }

    method statement_prefix:sym<start>($/) {
        self.attach: $/, self.r('StatementPrefix', 'Start').new($<blorst>.ast);
    }

    method blorst($/) {
        self.attach: $/, $<block> ?? $<block>.ast !! $<statement>.ast;
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
                    self.attach: $/, self.r('Ternary').new:
                        condition => $/[0].ast,
                        then => $/[1].ast,
                        else => $/[2].ast;
                }
                elsif $ast && nqp::istype($ast, self.r('DottyInfixish')) {
                    self.attach: $/, self.r('ApplyDottyInfix').new:
                        infix => $ast,
                        left => $/[0].ast,
                        right => $/[1].ast;
                }
                else {
                    self.attach: $/, self.r('ApplyInfix').new:
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
                self.attach: $/, self.r('ApplyListInfix').new:
                    infix => $ast,
                    operands => @operands;
            }
            elsif $KEY eq 'PREFIX' {
                self.attach: $/, self.r('ApplyPrefix').new:
                    prefix => $ast // self.r('Prefix').new($<prefix><sym>),
                    operand => $/[0].ast;
            }
            elsif $KEY eq 'POSTFIX' {
                if $<colonpair> {
                    $/[0].ast.add-colonpair($<colonpair>.ast);
                    make $/[0].ast;
                }
                else {
                    self.attach: $/, self.r('ApplyPostfix').new:
                        postfix => $ast // self.r('Postfix').new($<postfix><sym>),
                        operand => $/[0].ast;
                }
            }
            else {
                nqp::die("EXPR $KEY handling NYI");
            }
        }
        else {
            # Just a term.
            self.attach: $/, $ast;
        }
    }

    method prefixish($/) {
        my $ast := $<OPER>.ast // self.r('Prefix').new(~$<prefix><sym>);
        if $<prefix_postfix_meta_operator> {
            $ast := $<prefix_postfix_meta_operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method prefix_postfix_meta_operator:sym<«>($/) {
        make self.r('MetaPrefix', 'Hyper');
    }

    method postfixish($/) {
        my $ast := $<OPER>.ast // self.r('Postfix').new(~$<postfix><sym>);
        if $<postfix_prefix_meta_operator> {
            $ast := $<postfix_prefix_meta_operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method postfix_prefix_meta_operator:sym<»>($/) {
        # Check if we are inside «...» quoters and complain if the hyper creates
        # ambiguity with the quoters, since user may not wanted to have a hyper
        my str $sym := ~$<sym>;
        if ($/.pragma("STOPPER") // '') eq $sym {
            $/.worry:
                "Ambiguous use of $sym; use "
                ~ ($<sym> eq '>>' ?? '»' !! '>>')
                ~ " instead to mean hyper, or insert whitespace before"
                ~ " $sym to mean a quote terminator (or use different delimiters?)";
        }

        make self.r('MetaPostfix', 'Hyper');
    }

    method postop($/) {
        self.attach: $/, $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
    }

    method postcircumfix:sym<[ ]>($/) {
        self.attach: $/, self.r('Postcircumfix', 'ArrayIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<{ }>($/) {
        self.attach: $/, self.r('Postcircumfix', 'HashIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<ang>($/) {
        self.attach: $/, self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym«<< >>»($/) {
        self.attach: $/, self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym<« »>($/) {
        self.attach: $/, self.r('Postcircumfix', 'LiteralHashIndex').new($<nibble>.ast);
    }

    method postcircumfix:sym<( )>($/) {
        self.attach: $/, self.r('Call', 'Term').new(args => $<arglist>.ast);
    }

    method dotty:sym<.>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.^>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.?>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.&>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dottyop($/) {
        if $<methodop> {
            self.attach: $/, $<methodop>.ast;
        }
        elsif $<postop> {
            self.attach: $/, $<postop>.ast;
        }
        else {
            nqp::die('NYI kind of dottyop');
        }
    }

    method methodop($/) {
        my $args := $<args> ?? $<args>.ast !! self.r('ArgList').new();
        if $<longname> {
            if $*special {
                my $longname := $<longname>.ast;
                unless $longname.is-identifier {
                    $/.dotty-non-ident($*special);
                }
                my $name := $longname.canonicalize;
                if $*special eq '.^' {
                    self.attach: $/, self.r('Call', 'MetaMethod').new(:$name, :$args);
                }
                elsif $*special eq '.?' {
                    self.attach: $/, self.r('Call', 'MaybeMethod').new(:$name, :$args);
                }
                elsif $*special eq '.&' {
                    self.attach: $/, self.r('Call', 'VarMethod').new(:name($<longname>.ast), :$args);
                }
                else {
                    nqp::die("Missing compilation of $*special");
                }
            }
            else {
                self.attach: $/, self.r('Call', 'Method').new(:name($<longname>.ast), :$args);
            }
        }
        elsif $<quote> {
            self.attach: $/, self.r('Call', 'QuotedMethod').new(:name($<quote>.ast), :$args);
        }
        else {
            nqp::die('NYI kind of methodop');
        }
    }

    method postfix:sym<ⁿ>($/) {
        my $Int := $*LITERALS.int-type;
        my $power := nqp::box_i(0, $Int);
        for $<dig> {
            $power := nqp::add_I(
                nqp::mul_I($power, nqp::box_i(10, $Int), $Int),
                nqp::box_i(nqp::index("⁰¹²³⁴⁵⁶⁷⁸⁹", $_), $Int),
                $Int);
        }
        $power := nqp::neg_I($power, $Int) if $<sign> eq '⁻' || $<sign> eq '¯';
        self.attach: $/, self.r('Postfix', 'Power').new($power);
    }

    method infixish($/) {
        return 0 if $<fake_infix>;

        my $ast;
        if $<infix> {
            $ast := $<infix>.ast;
            unless $ast || $<infix><sym> eq '??' {
                $ast := self.r('Infix').new(~$<infix>);
            }
        }
        elsif $<infix_prefix_meta_operator> {
            $ast := $<infix_prefix_meta_operator>.ast;
        }
        elsif $<infix_circumfix_meta_operator> {
            $ast := $<infix_circumfix_meta_operator>.ast;
        }
        elsif $<infixish> {
            $ast := self.r('BracketedInfix').new($<infixish>.ast);
        }
        elsif $<variable> {
            $ast := self.r('FunctionInfix').new($<variable>.ast);
        }
        else {
            nqp::die('unknown kind of infix');
        }
        if $<infix_postfix_meta_operator> {
            $ast := $<infix_postfix_meta_operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method infix_prefix_meta_operator:sym<!>($/) {
        self.attach: $/, self.r('MetaInfix', 'Negate').new($<infixish>.ast);
    }

    method infix_prefix_meta_operator:sym<R>($/) {
        self.attach: $/, self.r('MetaInfix', 'Reverse').new($<infixish>.ast);
    }

    method revO($/) {
        my $O := nqp::clone($*FROM);
        if    $O<assoc> eq 'right' { $O<assoc> := 'left' }
        elsif $O<assoc> eq 'left'  { $O<assoc> := 'right' }
        make $O;
    }

    method infix_prefix_meta_operator:sym<X>($/) {
        self.attach: $/, self.r('MetaInfix', 'Cross').new($<infixish>.ast);
    }

    method infix_prefix_meta_operator:sym<Z>($/) {
        self.attach: $/, self.r('MetaInfix', 'Zip').new($<infixish>.ast);
    }

    method infix_postfix_meta_operator:sym<=>($/) {
        self.attach: $/, self.r('MetaInfix', 'Assign');
    }

    method infix_circumfix_meta_operator:sym<« »>($/) {
        self.attach: $/, self.r('MetaInfix', 'Hyper').new:
            infix => $<infixish>.ast,
            dwim-left => $<opening> eq '«',
            dwim-right => $<closing> eq '»'
    }

    method infix_circumfix_meta_operator:sym«<< >>»($/) {
        self.attach: $/, self.r('MetaInfix', 'Hyper').new:
            infix => $<infixish>.ast,
            dwim-left => $<opening> eq '<<',
            dwim-right => $<closing> eq '>>'
    }

    method circumfix:sym<( )>($/) {
        self.attach: $/, self.r('Circumfix', 'Parentheses').new($<semilist>.ast);
    }

    method circumfix:sym<[ ]>($/) {
        self.attach: $/, self.r('Circumfix', 'ArrayComposer').new($<semilist>.ast);
    }

    method circumfix:sym<{ }>($/) {
        self.attach: $/, $<pblock>.ast.block-or-hash;
    }

    method circumfix:sym<ang>($/) { self.attach: $/, $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { self.attach: $/, $<nibble>.ast; }

    method circumfix:sym<« »>($/) { self.attach: $/, $<nibble>.ast; }

    method infix:sym<.>($/) {
        self.attach: $/, self.r('DottyInfix', 'Call').new;
    }

    method infix:sym<.=>($/) {
        self.attach: $/, self.r('DottyInfix', 'CallAssign').new;
    }

    ##
    ## Terms
    ##

    method term:sym<::?IDENT>($/) {
        self.attach: $/, self.r('Var', 'Lexical', 'Constant').new(~$/);
    }

    method term:sym<self>($/) {
        self.attach: $/, self.r('Term', 'Self').new();
    }

    method term:sym<now>($/) {
        self.attach: $/, self.r('Term', 'Named').new('now');
    }

    method term:sym<time>($/) {
        self.attach: $/, self.r('Term', 'Named').new('time');
    }

    method term:sym<empty_set>($/) {
        self.attach: $/, self.r('Term', 'EmptySet').new();
    }

    method term:sym<rand>($/) {
        self.attach: $/, self.r('Term', 'Rand').new();
    }

    method term:sym<fatarrow>($/) {
        self.attach: $/, self.r('FatArrow').new:
            key => $*LITERALS.intern-str(~$<key>),
            value => $<val>.ast;
    }

    method term:sym<colonpair>($/) {
        self.attach: $/, $<colonpair>.ast;
    }

    method term:sym<variable>($/) {
        self.attach: $/, $<variable>.ast;
    }

    method term:sym<package_declarator>($/) {
        self.attach: $/, $<package_declarator>.ast;
    }

    method term:sym<scope_declarator>($/) {
        self.attach: $/, $<scope_declarator>.ast;
    }

    method term:sym<routine_declarator>($/) {
        self.attach: $/, $<routine_declarator>.ast;
    }

    method term:sym<multi_declarator>($/) {
        self.attach: $/, $<multi_declarator>.ast;
    }

    method term:sym<regex_declarator>($/){
        self.attach: $/, $<regex_declarator>.ast;
    }

    method term:sym<statement_prefix>($/) {
        self.attach: $/, $<statement_prefix>.ast;
    }

    method term:sym<*>($/) {
        self.attach: $/, self.r('Term', 'Whatever').new;
    }

    method term:sym<**>($/) {
        self.attach: $/, self.r('Term', 'HyperWhatever').new;
    }

    method term:sym<lambda>($/) {
        self.attach: $/, $<pblock>.ast;
    }

    method term:sym<value>($/) {
        self.attach: $/, $<value>.ast;
    }

    method term:sym<identifier>($/) {
        self.attach: $/, self.r('Call', 'Name').new:
            name => self.r('Name').from-identifier(~$<identifier>),
            args => $<args>.ast; 
    }

    method term:sym<name>($/) {
        if $<args> {
            self.attach: $/, self.r('Call', 'Name').new:
                name => $<longname>.ast,
                args => $<args>.ast;
        }
        else {
            if $*is-type {
                self.attach: $/, self.r('Type', 'Simple').new($<longname>.ast);
            }
            else {
                self.attach: $/, self.r('Term', 'Name').new($<longname>.ast);
            }
        }
    }

    method term:sym<dotty>($/) {
        self.attach: $/, self.r('Term', 'TopicCall').new($<dotty>.ast);
    }

    method term:sym<capture>($/) {
        self.attach: $/, self.r('Term', 'Capture').new($<args>.ast);
    }

    method colonpair($/) {
        my $key-str := $*key;
        if $key-str {
            my $key := $*LITERALS.intern-str($key-str);
            if $<num> {
                my $value := self.r('IntLiteral').new($*LITERALS.intern-int(~$<num>, 10));
                self.attach: $/, self.r('ColonPair', 'Number').new(:$key, :$value);
            }
            elsif $<coloncircumfix> {
                my $value := $<coloncircumfix>.ast;
                self.attach: $/, self.r('ColonPair', 'Value').new(:$key, :$value);
            }
            elsif $<var> {
                my $value := $<var>.ast; 
                self.attach: $/, self.r('ColonPair', 'Variable').new(:$key, :$value);
            }
            elsif $<neg> {
                self.attach: $/, self.r('ColonPair', 'False').new(:$key);
            }
            else {
                self.attach: $/, self.r('ColonPair', 'True').new(:$key);
            }
        }
        else {
            make $<coloncircumfix>.ast;
        }
    }

    method coloncircumfix($/) {
        self.attach: $/, $<circumfix>
            ?? $<circumfix>.ast
            !! self.r('Term', 'Name').new(self.r('Name').from-identifier('Nil'));
    }

    method colonpair_variable($/) {
        if $<capvar> {
            self.attach: $/, self.r('Var', 'NamedCapture').new($*LITERALS.intern-str(~$<desigilname>));
        }
        else {
            my str $sigil := ~$<sigil>;
            my str $twigil := $<twigil> ?? ~$<twigil> !! '';
            my str $desigilname := ~$<desigilname>;
            self.compile_variable_access($/, $sigil, $twigil, $desigilname, $<desigilname><longname>);
        }
    }

    method variable($/) {
        if $<index> {
            self.attach: $/, self.r('Var', 'PositionalCapture').new($*LITERALS.intern-int(~$<index>, 10));
        }
        elsif $<postcircumfix> {
            self.attach: $/, self.r('Var', 'NamedCapture').new($<postcircumfix>.ast.index);
        }
        elsif $<contextualizer> {
            self.attach: $/, $<contextualizer>.ast;
        }
        elsif $<infixish> {
            my $name := self.r('Name').from-identifier('infix');
            $name.add-colonpair(
                self.r('QuotedString').new(
                    :segments($name.IMPL-WRAP-LIST([
                        RakuAST::StrLiteral.new($<infixish>.Str)
                    ]))
                )
            );
            self.compile_variable_access($/, '&', '', $name.canonicalize, '');
        }
        elsif $<desigilname><variable> {
            self.contextualizer-for-sigil($/, ~$<sigil>, $<desigilname><variable>.ast);
        }
        else {
            my str $sigil := ~$<sigil>;
            my str $twigil := $<twigil> ?? ~$<twigil> !! '';
            my str $desigilname := ~$<desigilname>;
            self.compile_variable_access($/, $sigil, $twigil, $desigilname, $<desigilname><longname>);
        }
    }

    method compile_variable_access($/, $sigil, $twigil, $desigilname, $longname) {
        my str $name := $sigil ~ $twigil ~ $desigilname;
        if $name eq $sigil {
            # Generate an anonymous state variable.
            self.attach: $/, self.r('VarDeclaration', 'Anonymous').new(:$sigil, :scope('state'));
        }
        elsif $name eq '@_' {
            my $decl := self.r('VarDeclaration', 'Placeholder', 'SlurpyArray').new();
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $name eq '%_' {
            my $decl := self.r('VarDeclaration', 'Placeholder', 'SlurpyHash').new();
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq '' {
            if !$longname || $longname<name>.ast.is-identifier {
                self.attach: $/, self.r('Var', 'Lexical').new($name);
            }
            else { # package variable
                self.attach: $/, self.r('Var', 'Package').new(
                    $longname<name>.ast,
                    :$sigil
                );
            }
        }
        elsif $twigil eq '*' {
            self.attach: $/, self.r('Var', 'Dynamic').new($name);
        }
        elsif $twigil eq '!' {
            self.attach: $/, self.r('Var', 'Attribute').new($name);
        }
        elsif $twigil eq '?' {
            if $name eq '$?FILE' {
                my str $file := self.current_file();
                self.attach: $/, self.r('Var', 'Compiler', 'File').new($*LITERALS.intern-str($file));
            }
            elsif $name eq '$?LINE' {
                my int $line := self.current_line($/);
                self.attach: $/, self.r('Var', 'Compiler', 'Line').new($*LITERALS.intern-int($line, 10));
            }
            else {
                self.attach: $/, self.r('Var', 'Compiler', 'Lookup').new($name);
            }
        }
        elsif $twigil eq '^' {
            my $decl := self.r('VarDeclaration', 'Placeholder', 'Positional').new:
                    $sigil ~ $desigilname;
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq ':' {
            my $decl := self.r('VarDeclaration', 'Placeholder', 'Named').new:
                    $sigil ~ $desigilname;
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq '=' {
            if $name eq '$=finish' {
                self.attach: $/, self.r('Var', 'Pod', 'Finish').new;
            }
            else {
                $/.typed_sorry('X::Comp::NYI', feature => 'Pod variable ' ~ $name);
            }
        }
        else {
            nqp::die("Lookup with twigil '$twigil' NYI");
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

    method contextualizer($/) {
        self.contextualizer-for-sigil($/, ~$<sigil>, $<coercee>.ast);
    }

    method contextualizer-for-sigil($/, $sigil, $target) {
        my str $node-type := $sigil eq '@' ?? 'List' !!
                             $sigil eq '%' ?? 'Hash' !!
                                              'Item';
        self.attach: $/, self.r('Contextualizer', $node-type).new($target);
    }

    method term:sym<reduce>($/) {
        my $infix := $<op>.ast // self.r('Infix').new($<op><OPER><sym>);
        self.attach: $/, self.r('Term', 'Reduce').new(:$infix, :args($<args>.ast),
            :triangle(?$<triangle>));
    }

    ##
    ## Declarations
    ##

    method package_declarator:sym<package>($/) { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<module>($/)  { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<class>($/)   { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<grammar>($/) { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<role>($/)    { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<knowhow>($/) { self.attach: $/, $<package_def>.ast; }
    method package_declarator:sym<native>($/)  { self.attach: $/, $<package_def>.ast; }

    method package_def($/) {
        my $package := $*PACKAGE;
        my $body := $<block>
            ?? $<block>.ast
            !! self.r('Block').new(body => self.r('Blockoid').new($<statementlist>.ast));
        if $<signature> {
            # upgrade body to a pointy block with a signature
            $body := self.r('PointyBlock').new(:signature($<signature>.ast), :body($body.body));
        }
        $package.replace-body: $body;
        self.attach: $/, $package;
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

        # Let the resolver know which package we're in.
        $*R.push-package($*PACKAGE);
    }

    method leave-package-scope($/) {
        $*R.pop-package();
    }

    method scope_declarator:sym<my>($/)    { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<our>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<has>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<HAS>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)  { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<state>($/) { self.attach: $/, $<scoped>.ast; }
    method scope_declarator:sym<unit>($/)  { self.attach: $/, $<scoped>.ast; }

    method scoped($/) {
        self.attach: $/, $<DECL>.ast;
    }

    method multi_declarator:sym<multi>($/) {
        self.attach: $/, $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast;
    }

    method multi_declarator:sym<proto>($/) {
        self.attach: $/, $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast;
    }

    method multi_declarator:sym<null>($/) {
        self.attach: $/, $<declarator>.ast;
    }

    method declarator($/) {
        if $<variable_declarator> {
            self.attach: $/, $<variable_declarator>.ast;
        }
        elsif $<signature> {
            my str $scope := $*SCOPE;
            my $type := $*OFTYPE ?? $*OFTYPE.ast !! self.r('Type');
            my $initializer := $<initializer>
                ?? $<initializer>.ast
                !! self.r('Initializer');
            self.attach: $/, self.r('VarDeclaration', 'Signature').new:
                :signature($<signature>.ast), :$scope, :$type, :$initializer;
        }
        elsif $<routine_declarator> {
            self.attach: $/, $<routine_declarator>.ast;
        }
        elsif $<defterm> {
            my str $scope := $*SCOPE;
            my $type := $*OFTYPE ?? $*OFTYPE.ast !! self.r('Type');
            my $name := $<defterm>.ast;
            my $initializer := $<term_init>.ast;
            my $decl := self.r('VarDeclaration', 'Term').new:
                :$scope, :$type, :$name, :$initializer;
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        else {
            nqp::die('Unimplemented declarator');
        }
    }

    method initializer:sym<=>($/) {
        self.attach: $/, self.r('Initializer', 'Assign').new($<EXPR>.ast);
    }

    method initializer:sym<:=>($/) {
        self.attach: $/, self.r('Initializer', 'Bind').new($<EXPR>.ast);
    }

    method variable_declarator($/) {
        my str $scope := $*SCOPE;
        my $type := $*OFTYPE ?? $*OFTYPE.ast !! self.r('Type');
        my $initializer := $<initializer>
            ?? $<initializer>.ast
            !! self.r('Initializer');
        my $decl;
        if $<desigilname> {
            my str $name := $<sigil> ~ ($<twigil> || '') ~ $<desigilname>;
            $decl := self.r('VarDeclaration', 'Simple').new:
                :$scope, :$type, :$name, :$initializer;
            if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
                $*R.declare-lexical($decl);
            }
        }
        else {
            if $scope ne 'my' && $scope ne 'state' {
                $/.panic("Cannot declare an anonymous {$scope}-scoped variable");
            }
            if $<twigil> {
                $/.panic("Cannot declare an anonymous variable with a twigil");
            }
            $decl := self.r('VarDeclaration', 'Anonymous').new:
                :$scope, :$type, :sigil(~$<sigil>), :$initializer;
        }
        for $<trait> {
            $decl.add-trait($_.ast);
        }
        self.attach: $/, $decl;
    }

    method routine_declarator:sym<sub>($/) {
        self.attach: $/, $<routine_def>.ast;
    }
    method routine_declarator:sym<method>($/) {
        self.attach: $/, $<method_def>.ast;
    }
    method routine_declarator:sym<submethod>($/) {
        self.attach: $/, $<method_def>.ast;
    }

    method routine_def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        $routine.replace-body($<onlystar> ?? $<onlystar>.ast !! $<blockoid>.ast);
        $routine.ensure-begin-performed($*R);
        self.attach: $/, $routine;
    }

    method method_def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        if $<specials> {
            if ~$<specials> eq '^' {
                $routine.set-meta(1);
            }
            elsif ~$<specials> eq '!' {
                $routine.set-private(1);
            }
        }
        $routine.replace-body($<onlystar> ?? $<onlystar>.ast !! $<blockoid>.ast);
        $routine.ensure-begin-performed($*R);
        self.attach: $/, $routine;
    }

    method regex_declarator:sym<regex>($/) {
        self.attach: $/, $<regex_def>.ast;
    }

    method regex_declarator:sym<token>($/) {
        self.attach: $/, $<regex_def>.ast;
    }

    method regex_declarator:sym<rule>($/) {
        self.attach: $/, $<regex_def>.ast;
    }

    method regex_def($/) {
        my $regex := $*BLOCK;
        if $<signature> {
            $regex.replace-signature($<signature>.ast);
        }
        $regex.replace-body($<nibble>.ast);
        $regex.ensure-begin-performed($*R);
        self.attach: $/, $regex;
    }

    method trait($/) {
        my $trait := $<trait_mod>.ast;
        if $trait { # is repr(...) won't be handled as a trait
            if $*TARGET {
                # Already have the target to apply it to.
                $*TARGET.add-trait($trait);
            }
            else {
                # Will be added to a target later, so attach to the match.
                self.attach: $/, $trait;
            }
        }
    }

    method trait_mod:sym<is>($/) {
        if ~$<longname> eq 'repr' {
            if $<circumfix> {
                my $repr := $<circumfix>.ast.IMPL-UNWRAP-LIST($<circumfix>.ast.semilist.statements)[0];
                unless $repr.IMPL-CAN-INTERPRET {
                    $/.typed_panic('X::Value::Dynamic', :what('is repr(...) trait'));
                }
                $repr := $repr.IMPL-INTERPRET(self.r('IMPL', 'InterpContext').new);
                $*PACKAGE.set-repr($repr);
                return;
            }
            else {
                $/.panic("is repr(...) trait needs a parameter");
            }
        }
        else
        {
            my $ast-type := self.r('Trait', 'Is');
            my $trait := $<circumfix>
                ?? $ast-type.new(:name($<longname>.ast), :argument($<circumfix>.ast))
                !! $ast-type.new(:name($<longname>.ast));
            $trait.ensure-begin-performed($*R);
            self.attach: $/, $trait;
        }
    }

    method trait_mod:sym<hides>($/) {
        self.attach: $/, self.r('Trait', 'Hides').new($<typename>.ast);
    }

    method trait_mod:sym<does>($/) {
        self.attach: $/, self.r('Trait', 'Does').new($<typename>.ast);
    }

    method trait_mod:sym<of>($/) {
        self.attach: $/, self.r('Trait', 'Of').new($<typename>.ast);
    }

    method trait_mod:sym<returns>($/) {
        self.attach: $/, self.r('Trait', 'Returns').new($<typename>.ast);
    }

    ##
    ## Values
    ##

    method value:sym<quote>($/) {
        self.attach: $/, $<quote>.ast;
    }

    method value:sym<number>($/) {
        self.attach: $/, $<number>.ast;
    }

    method value:sym<version>($/) {
        self.attach: $/, $<version>.ast;
    }

    method number:sym<numish>($/) {
        self.attach: $/, $<numish>.ast;
    }

    method numish($/) {
        if $<integer> {
            self.attach: $/, self.r('IntLiteral').new($<integer>.ast);
        }
        elsif $<dec_number> {
            self.attach: $/, $<dec_number>.ast;
        }
        elsif $<rad_number> {
            self.attach: $/, $<rad_number>.ast;
        }
        elsif $<rat_number> {
            self.attach: $/, $<rat_number>.ast;
        }
        elsif $<unum> {
            my $code := nqp::ord($/.Str);
            my int $nu := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Numerator"));
            my int $de := +nqp::getuniprop_str($code, nqp::unipropcode("Numeric_Value_Denominator"));
            if !$de || $de == 1 {
                self.attach: $/, self.r('IntLiteral').new($*LITERALS.intern-int($nu, 10));
            }
            else {
                self.attach: $/, self.r('RatLiteral').new($*LITERALS.intern-rat(
                    $*LITERALS.intern-int($nu, 10),
                    $*LITERALS.intern-int($de, 10)
                ));
            }
        }
        elsif $<uinf> {
            self.attach: $/, self.r('NumLiteral').new($*LITERALS.intern-num('Inf'));
        }
        else {
            self.attach: $/, self.r('NumLiteral').new($*LITERALS.intern-num(~$/));
        }
    }

    method decint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 10, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method hexint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 16, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method octint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 8, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method binint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 2, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method signed-integer($/) {
        my $integer := $<integer>.ast;
        self.attach: $/, $<sign> eq '-' || $<sign> eq '−'
            ?? nqp::neg_I($integer, $integer.WHAT)
            !! $integer;
    }

    method dec_number($/) {
        if $<escale> { # wants a Num
            self.attach: $/, self.r('NumLiteral').new($*LITERALS.intern-num(~$/));
        }
        else { # wants a Rat
            self.attach: $/, self.r('RatLiteral').new($*LITERALS.intern-rat(
                $<int> ?? $<int>.ast !! NQPMu,
                ~$<frac>));
        }
    }

    method rad_number($/) {
        my $literals := $*LITERALS;
        if $<bracket> {
            self.attach: $/, self.r('Term', 'RadixNumber').new:
                :radix($literals.intern-int(~$<radix>, 10)),
                :value($<bracket>.ast),
                :multi-part;
        }
        elsif $<circumfix> {
            self.attach: $/, self.r('Term', 'RadixNumber').new:
                :radix($literals.intern-int(~$<radix>, 10)),
                :value($<circumfix>.ast);
        }
        else {
            # Check and override $radix if necessary.
            my int $radix := nqp::radix(10, $<radix>, 0, 0)[0];
            $/.typed_panic('X::Syntax::Number::RadixOutOfRange', :$radix)
                unless (2 <= $radix) && ($radix <= 36);
            if nqp::chars($<ohradix>) {
                my $ohradstr := $<ohradix>.Str;
                if $ohradstr eq "0x" {
                    $radix := 16;
                } elsif $ohradstr eq "0o" {
                    $radix := 8;
                } elsif $ohradstr eq "0d" {
                    $radix := 10;
                } elsif $ohradstr eq "0b" {
                    $radix := 2;
                } else {
                    $/.panic("Unknown radix prefix '$ohradstr'.");
                }
            }

            # Parse and assemble number.
            my $Int := $literals.int-type;
            my $Num := $literals.num-type;
            my $ipart := nqp::radix_I($radix, $<intpart>.Str, 0, 0, $Int);
            my $fpart := nqp::radix_I($radix, nqp::chars($<fracpart>) ?? $<fracpart>.Str !! ".0", 1, 4, $Int);
            my $bpart := $<base> ?? nqp::tonum_I($<base>[0].ast) !! $radix;
            my $epart := $<exp> ?? nqp::tonum_I($<exp>[0].ast) !! 0;

            if $ipart[2] < nqp::chars($<intpart>.Str) {
                $/.typed_panic: 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :pos($ipart[2] < 0 ?? 0 !! $ipart[2]),
                    :reason("malformed base-$radix number");
            }
            if $fpart[2] < nqp::chars($<fracpart>.Str) {
                $/.typed_panic: 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :reason("malformed base-$radix number"),
                    :pos( # the -1 dance is due to nqp::radix returning -1 for
                        # failure to parse the first char, instead of 0;
                        # we return `1` to cover the decimal dot in that case
                        $ipart[2] + ($fpart[2] == -1 ?? 1 !! $fpart[2])
                    );
            }

            my $base := nqp::pow_I(nqp::box_i($radix, $Int), $fpart[1], $Num, $Int);
            $ipart := nqp::mul_I($ipart[0], $base, $Int);
            $ipart := nqp::add_I($ipart, $fpart[0], $Int);
            $fpart := $base;

            my $scientific := nqp::pow_n($bpart, $epart);
            $ipart := nqp::mul_I($ipart, nqp::fromnum_I($scientific, $Int), $Int);

            if $fpart != 1 { # non-unit fractional part, wants Rat
                self.attach: $/, self.r('RatLiteral').new($literals.intern-rat($ipart, $fpart));
            }
            else { # wants Int
                self.attach: $/, self.r('IntLiteral').new($ipart);
            }
        }
    }

    method rat_number($/) {
        self.attach: $/, $<bare_rat_number>.ast;
    }

    method bare_rat_number($/) {
        self.attach: $/, self.r('RatLiteral').new($*LITERALS.intern-rat($<nu>.ast, $<de>.ast));
    }

    method version($/) {
        # We don't self.attach: $/, an object for the initial language version line,
        # which occurs before a setting is loaded.
        if $*R {
            my $Version := $*R.resolve-lexical-constant('Version').compile-time-value;
            self.attach: $/, self.r('VersionLiteral').new($Version.new(~$<vstr>));
        }
    }

    method quote:sym<apos>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<sapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<lapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<hapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<dblq>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<sdblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<ldblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<hdblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<crnr>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<qq>($/)    { self.attach: $/, $<quibble>.ast; }
    method quote:sym<q>($/)     { self.attach: $/, $<quibble>.ast; }
    method quote:sym<Q>($/)     { self.attach: $/, $<quibble>.ast; }

    method quote:sym</ />($/) {
        self.attach: $/, self.r('QuotedRegex').new(body => $<nibble>.ast);
    }

    method quote:sym<rx>($/) {
        self.attach: $/, self.r('QuotedRegex').new:
            body => $<quibble>.ast,
            adverbs => $<rx_adverbs>.ast;
    }

    method quote:sym<m>($/) {
        self.attach: $/, self.r('QuotedRegex').new: :match-immediately,
            body => $<quibble>.ast,
            adverbs => $<rx_adverbs>.ast;
    }

    method quote:sym<s>($/) {
        self.attach: $/, self.r('Substitution').new:
            immutable => $<sym> eq 'S',
            samespace => ?$/[0],
            adverbs => $<rx_adverbs>.ast,
            pattern => $<sibble><left>.ast,
            infix => $<sibble><infixish> ?? $<sibble><infixish>.ast !! self.r('Infixish'),
            replacement => $<sibble><right>.ast;
    }

    # We make a list of the quotepairs to attach them to the regex
    # construct; validation of what is valid takes place in the AST.
    # However, a limited number of them are required for parsing the
    # regex and constructing its AST correctly. Of note, these are
    # s (sigspace, as it controls how whitespce is parsed), m (so we
    # can construct character class ranges correctly), and P5 (Perl5,
    # so we know which regex language to parse). These get special
    # handling.
    my %RX_ADVERB_COMPILE := nqp::hash('s', 1, 'm', 1, 'P5', 1);
    my %RX_ADVERB_COMPILE_CANON := nqp::hash(
        'sigspace', 's',
        'ignoremark', 'm',
        'Perl5', 'P5',
        'ss', 's',
        'samespace', 's',
        'mm', 'm',
        'samemark', 'm');
    method rx_adverbs($/) {
        my @pairs;
        for $<quotepair> {
            my $ast := $_.ast;
            @pairs.push($ast);
            my str $key := $ast.key;
            my str $canon := %RX_ADVERB_COMPILE_CANON{$key} // $key;
            if %RX_ADVERB_COMPILE{$canon} {
                my $value := $ast.simple-compile-time-quote-value();
                if nqp::isconcrete($value) {
                    %*RX{$canon} := $value ?? 1 !! 0;
                }
                else {
                    $_.typed_panic('X::Value::Dynamic', what => "Adverb $key");
                }
            }
        }
        make @pairs;
    }

    method quotepair($/) {
        my $key := $*LITERALS.intern-str($*key);
        if $<num> {
            my $value := self.r('IntLiteral').new($*LITERALS.intern-int(~$<num>, 10));
            self.attach: $/, self.r('ColonPair', 'Number').new(:$key, :$value);
        }
        elsif $<circumfix> {
            my $value := $<circumfix>.ast;
            self.attach: $/, self.r('ColonPair', 'Value').new(:$key, :$value);
        }
        elsif $<neg> {
            self.attach: $/, self.r('ColonPair', 'False').new(:$key);
        }
        else {
            self.attach: $/, self.r('ColonPair', 'True').new(:$key);
        }
    }

    ##
    ## Types
    ##

    method typename($/) {
        my $base-name := $<longname>
            ?? $<longname>.ast
            !! self.r('Name').from-identifier('::?' ~ $<identifier>);
        for $<colonpair> {
            $base-name.add-colonpair($_.ast);
        }
        my str $str_longname := ~$<longname>;
        if nqp::eqat($str_longname, '::', 0) {
            if $<arglist> || $<typename> {
                $/.panic("Cannot put type parameters on a type capture");
            }
            if $<accepts> || $<accepts_any> {
                $/.panic("Cannot base a coercion type on a type capture");
            }
            if $str_longname eq '::' {
                $/.panic("Cannot use :: as a type name");
            }
            my $type-capture := self.r('Type', 'Capture').new($base-name);
            self.attach: $/, $type-capture;

            # Declare the lexical so it is available right away (e.g. for traits)
            $*R.declare-lexical($type-capture);
        }
        elsif $<accept> {
            self.attach: $/, self.r('Type', 'Coercion').new($base-name, $<accept>.ast);
        }
        elsif $<accept_any> {
            my $Any := self.r('Type', 'Setting').new(RakuAST::Name.from-identifier('Any'));
            self.attach: $/, self.r('Type', 'Coercion').new($base-name, $Any);
        }
        elsif $base-name.has-colonpair('D') {
            self.attach: $/, self.r('Type', 'Definedness').new($base-name, 1);
        }
        elsif $base-name.has-colonpair('U') {
            self.attach: $/, self.r('Type', 'Definedness').new($base-name, 0);
        }
        else {
            self.attach: $/, self.r('Type', 'Simple').new($base-name);
        }
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
        my $returns;
        if $<typename> {
            $returns := $<typename>.ast;
        }
        elsif $<value> {
            $returns := $<value>.ast;
            unless nqp::istype($returns, self.r('CompileTimeValue')) {
                $<value>.panic('Return value after --> may only be a type or a constant');
            }
        }
        else {
            $returns := self.r('Node');
        }
        self.attach: $/, self.r('Signature').new(:@parameters, :$returns);
    }

    method parameter($/) {
        my $parameter := $<param_var>   ?? $<param_var>.ast   !!
                         $<named_param> ?? $<named_param>.ast !!
                         $<param_term>  ?? $<param_term>.ast  !!
                         self.r('Parameter').new;
        my $capture := self.r('Type', 'Capture');
        for $<type_constraint> {
            my $type-constraint := $_.ast;
            if nqp::istype($type-constraint, $capture) {
                $parameter.add-type-capture($type-constraint);
            }
            else {
                $parameter.set-type($type-constraint);
            }
        }
        if $<quant> {
            my str $q := ~$<quant>;
            $parameter.set-optional() if $q eq '?';
            $parameter.set-required() if $q eq '!';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Flattened'))
                if $q eq '*';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Unflattened'))
                if $q eq '**';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'SingleArgument'))
                if $q eq '+';
            $parameter.set-slurpy(self.r('Parameter', 'Slurpy', 'Capture'))
                if $q eq '|';
        }
        for $<trait> {
            $parameter.add-trait($_.ast);
        }
        if $<default_value> {
            $parameter.set-default($<default_value>.ast);
        }
        if $<post_constraint> {
            if $<post_constraint>[0]<EXPR> {
                $parameter.set-where($<post_constraint>[0].ast);
            }
        }
        self.attach: $/, $parameter;
    }

    method param_var($/) {
        # Work out what kind of thing we're binding into, if any.
        my %args;
        if $<name> {
            my $decl := self.r('ParameterTarget', 'Var').new(~$<declname>);
            $*R.declare-lexical($decl);
            %args<target> := $decl;
        }
        elsif $<signature> {
            %args<sub-signature> := $<signature>.ast;
        }

        # Build the parameter.
        self.attach: $/, self.r('Parameter').new(|%args);
    }

    method param_term($/) {
        if $<defterm> {
            # Create sigilless target to bind into
            my $decl := self.r('ParameterTarget', 'Term').new($<defterm>.ast);
            $*R.declare-lexical($decl);
            self.attach: $/, self.r('Parameter').new(target => $decl);
        }
        else {
            # Anonymous
            self.attach: $/, self.r('Parameter').new();
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
            }
            $parameter.add-name(~$<name>);
        }
        else {
            # Name comes from the parameter variable.
            $parameter := $<param_var>.ast;
            my $name-match := $<param_var><name>;
            $parameter.add-name($name-match ?? ~$name-match !! '');
        }
        self.attach: $/, $parameter;
    }

    method default_value($/) {
        make $<EXPR>.ast;
    }

    method type_constraint($/) {
        self.attach: $/, $<typename>.ast;
    }

    method post_constraint($/) {
        if $<EXPR> {
            make $<EXPR>.ast;
        }
        elsif $<signature> {
            make $<signature>.ast;
        }
    }

    ##
    ## Argument lists and captures
    ##

    method args($/) {
        if    $<semiarglist> { self.attach: $/, $<semiarglist>.ast; }
        elsif $<arglist>     { self.attach: $/, $<arglist>.ast; }
        else                 { self.attach: $/, self.r('ArgList').new(); }
    }

    method semiarglist($/) {
        if nqp::elems($<arglist>) == 1 {
            self.attach: $/, $<arglist>[0].ast;
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
                self.attach: $/, self.r('ArgList').from-comma-list($expr);
            }
            else {
                self.attach: $/, self.r('ArgList').new($expr);
            }
        }
        else {
            self.attach: $/, self.r('ArgList').new();
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
            self.attach: $/, self.r('Name').new(|@parts);
        }
        else {
            self.attach: $/, self.r('Name').from-identifier(~$<identifier>);
        }
    }

    method morename($/) {
        if $<identifier> {
            self.attach: $/, self.r('Name', 'Part', 'Simple').new(~$<identifier>);
        }
        elsif $<EXPR> {
            my $expr := $<EXPR>.ast;
            self.attach: $/, self.r('Name', 'Part', 'Expression').new($expr);
        }
        else {
            self.attach: $/, self.r('Name', 'Part', 'Empty');
        }
    }

    method longname($/) {
        my $name := $<name>.ast;
        for $<colonpair> {
            $name.add-colonpair($_.ast);
        }
        self.attach: $/, $name;
    }

    method deflongname($/) {
        # Set the name on the definition immediately, since it's known at this
        # point onwards.
        my $name := $<name>.ast;
        for $<colonpair> {
            $name.add-colonpair($_.ast);
        }
        $*BLOCK.replace-name($name);

        # Register it with the resolver.
        my $scope := $*SCOPE || $*DEFAULT-SCOPE;
        $*BLOCK.replace-scope($scope);
        if $*MULTINESS ne 'multi' {
            if $scope eq 'my' {
                $*R.declare-lexical-in-outer($*BLOCK);
            }
            elsif $*DEFAULT-SCOPE ne 'has' {
                $*R.declare-lexical($*BLOCK);
            }
        }
    }

    method defterm($/) {
        self.attach: $/, self.r('Name').from-identifier(~$/);
    }

    method pod_block:sym<finish>($/) {
        $*CU.replace-finish-content($*LITERALS.intern-str(~$<finish>));
    }
}

class Raku::QActions is HLL::Actions does Raku::CommonActions {
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
        self.attach: $/, $codepoint;
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
        my $node-class := nqp::can($/, 'herelang') ?? 'Heredoc' !! 'QuotedString';
        self.attach: $/, self.r($node-class).new(:@segments, :@processors);
    }

    method escape:sym<\\>($/) { self.attach: $/, $<item>.ast; }
    method backslash:sym<qq>($/) { self.attach: $/, $<quote>.ast; }
    method backslash:sym<\\>($/) { self.attach: $/, $<text>.Str; }
    method backslash:delim ($/) { self.attach: $/, $<text>.Str; }
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

    method escape:sym<$>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<@>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<%>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<&>($/) { self.attach: $/, $<EXPR>.ast; }

    method escape:sym<{ }>($/) {
        self.attach: $/, $<block>.ast;
    }

    method escape:sym<'>($/) { self.attach: $/, self.qwatom($<quote>.ast); }
    method escape:sym<colonpair>($/) { self.attach: $/, self.qwatom($<colonpair>.ast); }
    method escape:sym<#>($/) { make ''; }
    method qwatom($ast) { self.r('QuoteWordsAtom').new($ast) }
}

class Raku::RegexActions is HLL::Actions does Raku::CommonActions {
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
        self.attach: $/, $<termseq>.ast;
    }

    method termseq($/) {
        self.attach: $/, $<termaltseq>.ast;
    }

    method termaltseq($/) {
        if nqp::elems($<termconjseq>) == 1 {
            self.attach: $/, $<termconjseq>[0].ast;
        }
        else {
            my @branches;
            for $<termconjseq> {
                @branches.push($_.ast);
            }
            self.attach: $/, self.r('Regex', 'SequentialAlternation').new(|@branches);
        }
    }

    method termconjseq($/) {
        if nqp::elems($<termalt>) == 1 {
            self.attach: $/, $<termalt>[0].ast;
        }
        else {
            my @branches;
            for $<termalt> {
                @branches.push($_.ast);
            }
            self.attach: $/, self.r('Regex', 'SequentialConjunction').new(|@branches);
        }
    }

    method termalt($/) {
        if nqp::elems($<termconj>) == 1 {
            self.attach: $/, $<termconj>[0].ast;
        }
        else {
            my @branches;
            for $<termconj> {
                @branches.push($_.ast);
            }
            self.attach: $/, self.r('Regex', 'Alternation').new(|@branches);
        }
    }

    method termconj($/) {
        if nqp::elems($<termish>) == 1 {
            self.attach: $/, $<termish>[0].ast;
        }
        else {
            my @branches;
            for $<termish> {
                @branches.push($_.ast);
            }
            self.attach: $/, self.r('Regex', 'Conjunction').new(|@branches);
        }
    }

    method termish($/) {
        if nqp::elems($<noun>) == 1 {
            self.attach: $/, $<noun>[0].ast;
        }
        else {
            my @terms;
            for $<noun> {
                @terms.push($_.ast);
            }
            self.attach: $/, self.r('Regex', 'Sequence').new(|@terms);
        }
    }

    method quantified_atom($/) {
        my $atom := self.wrap-sigspace: $<sigmaybe>, $<atom>.ast;
        if $<quantifier> {
            my $quantifier := $<quantifier>.ast;
            if $<separator> {
                my $separator := $<separator>.ast;
                my $trailing-separator := $<separator><septype> eq '%%';
                self.attach: $/, self.wrap-sigspace: $<sigfinal>,
                    self.r('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier,
                        :$separator, :$trailing-separator);
            }
            else {
                self.attach: $/, self.wrap-sigspace: $<sigfinal>,
                    self.r('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier);
            }
        }
        else {
            if $<separator> {
                $/.panic("'" ~ $<separator><septype> ~
                    "' may only be used immediately following a quantifier");
            }
            if $<backmod> {
                self.attach: $/, self.wrap-sigspace: $<sigfinal>,
                    self.r('Regex', 'BacktrackModifiedAtom').new:
                        :$atom, :backtrack($<backmod>.ast);
            }
            else {
                self.attach: $/, self.wrap-sigspace: $<sigfinal>, $atom;
            }
        }
    }

    method wrap-sigspace($cond, $ast) {
        $cond && $cond.ast
            ?? self.r('Regex', 'WithSigspace').new($ast)
            !! $ast
    }

    method sigmaybe:sym<normspace>($/) { make 0; }
    method sigmaybe:sym<sigwhite>($/) { make 1; }

    method atom($/) {
        if $<metachar> {
            self.attach: $/, $<metachar>.ast;
        }
        else {
            self.attach: $/, self.r('Regex', 'Literal').new(~$/);
        }
    }

    method quantifier:sym<*>($/) {
        self.attach: $/, self.r('Regex', 'Quantifier', 'ZeroOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<+>($/) {
        self.attach: $/, self.r('Regex', 'Quantifier', 'OneOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<?>($/) {
        self.attach: $/, self.r('Regex', 'Quantifier', 'ZeroOrOne').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<**>($/) {
        if $<codeblock> {
            self.attach: $/, self.r('Regex', 'Quantifier', 'BlockRange').new:
                :block($<codeblock>.ast), :backtrack($<backmod>.ast);
        }
        else {
            my $min := $<min>
                ?? $*LITERALS.build-int(~$<min>, 10)
                !! $*LITERALS.int-type;
            my $max;
            if !$<max> {
                $max := $min;
            }
            elsif $<max> eq '*' {
                $max := $*LITERALS.int-type;
            }
            else {
                $max := $*LITERALS.build-int(~$<max>, 10);
            }
            my int $excludes-min := $<from> eq '^';
            my int $excludes-max := $<upto> eq '^';
            self.attach: $/, self.r('Regex', 'Quantifier', 'Range').new:
                :$min, :$max, :$excludes-min, :$excludes-max,
                :backtrack($<backmod>.ast);
        }
    }

    method backmod($/) {
        my str $backmod := ~$/;
        if $backmod eq ':' {
            self.attach: $/, self.r('Regex', 'Backtrack', 'Ratchet')
        }
        elsif $backmod eq ':?' || $backmod eq '?' {
            self.attach: $/, self.r('Regex', 'Backtrack', 'Frugal')
        }
        elsif $backmod eq ':!' || $backmod eq '!' {
            self.attach: $/, self.r('Regex', 'Backtrack', 'Greedy')
        }
        else {
            self.attach: $/, self.r('Regex', 'Backtrack')
        }
    }

    method separator($/) {
        self.attach: $/, $<quantified_atom>.ast;
    }

    method metachar:sym<[ ]>($/) {
        self.attach: $/, self.r('Regex', 'Group').new($<nibbler>.ast);
    }

    method metachar:sym<( )>($/) {
        self.attach: $/, self.r('Regex', 'CapturingGroup').new($<nibbler>.ast);
    }

    method metachar:sym<.>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'Any').new;
    }

    method metachar:sym<^>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'BeginningOfString').new;
    }

    method metachar:sym<^^>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'BeginningOfLine').new;
    }

    method metachar:sym<$>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'EndOfString').new;
    }

    method metachar:sym<$$>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'EndOfLine').new;
    }

    method metachar:sym<lwb>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'LeftWordBoundary').new;
    }

    method metachar:sym<rwb>($/) {
        self.attach: $/, self.r('Regex', 'Anchor', 'RightWordBoundary').new;
    }

    method metachar:sym<from>($/) {
        self.attach: $/, self.r('Regex', 'MatchFrom').new;
    }

    method metachar:sym<to>($/) {
        self.attach: $/, self.r('Regex', 'MatchTo').new;
    }

    method metachar:sym<bs>($/) {
        self.attach: $/, $<backslash>.ast;
    }

    method metachar:sym<mod>($/) {
        self.attach: $/, $<mod_internal>.ast;
    }

    method metachar:sym<assert>($/) {
        self.attach: $/, $<assertion>.ast;
    }

    method metachar:sym<:my>($/) {
        self.attach: $/, self.r('Regex', 'Statement').new($<statement>.ast);
    }

    method metachar:sym<{ }>($/) {
        self.attach: $/, self.r('Regex', 'Block').new($<codeblock>.ast);
    }

    method metachar:sym<var>($/) {
        if $<quantified_atom> {
            self.attach: $/, self.r('Regex', 'NamedCapture').new:
                name => ~($<name> || $<pos>),
                array => $<wantarray> ?? 1 !! 0,
                regex => $<quantified_atom>[0].ast;
        }
        elsif $<pos> {
            self.attach: $/, self.r('Regex', 'BackReference', 'Positional').new: +$<pos>;
        }
        else {
            self.attach: $/, self.r('Regex', 'BackReference', 'Named').new: ~$<name>;
        }
    }

    method metachar:sym<rakvar>($/) {
        if $<var><sigil> eq '%' {
            $<var>.typed_panic('X::Syntax::Reserved', :reserved('use of hash variables in regexes'))
        }
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, self.r('Regex', 'Interpolation').new(:var($<var>.ast), :$sequential);
    }

    method metachar:sym<qw>($/) {
        self.attach: $/, self.r('Regex', 'Quote').new($<nibble>.ast);
    }

    method metachar:sym<'>($/) {
        self.attach: $/, self.r('Regex', 'Quote').new($<quote>.ast);
    }

    method backslash:sym<e>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'Escape').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<f>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'FormFeed').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<h>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'HorizontalSpace').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<r>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'CarriageReturn').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<s>($/) {
        my constant NAME := nqp::hash('d', 'Digit', 'n', 'Newline', 's', 'Space', 'w', 'Word');
        self.attach: $/, self.r('Regex', 'CharClass', NAME{nqp::lc(~$<sym>)}).new(negated => $<sym> le 'Z');
    }

    method backslash:sym<t>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'Tab').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<v>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'VerticalSpace').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<0>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'Nul').new();
    }

    method backslash:sym<o>($/) {
        my str $characters := HLL::Actions.ints_to_string($<octint> || $<octints><octint>);
        self.attach: $/, self.r('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :$characters
    }

    method backslash:sym<x>($/) {
        my str $characters := HLL::Actions.ints_to_string($<hexint> || $<hexints><hexint>);
        self.attach: $/, self.r('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :$characters
    }

    method backslash:sym<c>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :characters($<charspec>.ast)
    }

    method backslash:sym<misc>($/) {
        self.attach: $/, self.r('Regex', 'Literal').new(~$/);
    }

    method assertion:sym<?>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            self.attach: $/, self.r('Regex', 'Assertion', 'Lookahead').new(:$assertion);
        }
        else {
            self.attach: $/, self.r('Regex', 'Assertion', 'Pass').new();
        }
    }

    method assertion:sym<!>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            self.attach: $/, self.r('Regex', 'Assertion', 'Lookahead').new(:$assertion, :negated);
        }
        else {
            self.attach: $/, self.r('Regex', 'Assertion', 'Fail').new();
        }
    }

    method assertion:sym<method>($/) {
        my $ast := $<assertion>.ast;
        if nqp::can($ast, 'set-capturing') {
            $ast.set-capturing(0);
        }
        self.attach: $/, $ast;
    }

    method assertion:sym<name>($/) {
        my $name := ~$<longname>;
        my $qast;
        if $<assertion> {
            my str $name := ~$<longname>;
            my $assertion := $<assertion>.ast;
            self.attach: $/, self.r('Regex', 'Assertion', 'Alias').new(:$name, :$assertion);
        }
        else {
            my $name := $<longname>.ast;
            if $<arglist> {
                my $args := $<arglist>.ast;
                self.attach: $/, self.r('Regex', 'Assertion', 'Named', 'Args').new(
                    :$name, :capturing, :$args);
            }
            elsif $<nibbler> {
                my $regex-arg := $<nibbler>.ast;
                self.attach: $/, self.r('Regex', 'Assertion', 'Named', 'RegexArg').new(
                    :$name, :capturing, :$regex-arg);
            }
            else {
                self.attach: $/, self.r('Regex', 'Assertion', 'Named').new(:$name, :capturing);
            }
        }
    }

    method assertion:sym<{ }>($/) {
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, self.r('Regex', 'Assertion', 'InterpolatedBlock').new:
            :block($<codeblock>.ast), :$sequential;
    }

    method assertion:sym<?{ }>($/) {
        self.attach: $/, self.r('Regex', 'Assertion', 'PredicateBlock').new:
            :block($<codeblock>.ast);
    }

    method assertion:sym<!{ }>($/) {
        self.attach: $/, self.r('Regex', 'Assertion', 'PredicateBlock').new:
            :negated, :block($<codeblock>.ast);
    }

    method assertion:sym<var>($/) {
        if $<call> {
            my $node := self.r('Regex', 'Assertion', 'Callable');
            self.attach: $/, $<arglist>
                ?? $node.new(:callee($<call>.ast), :args($<arglist>.ast))
                !! $node.new(:callee($<call>.ast));
        }
        else {
            my $sequential := $*SEQ ?? 1 !! 0;
            self.attach: $/, self.r('Regex', 'Assertion', 'InterpolatedVar').new:
                :var($<var>.ast), :$sequential;
        }
    }

    method assertion:sym<[>($/) {
        my @elems := $<cclass_elem>;
        my @asts;
        my int $i := 0;
        my int $n := nqp::elems(@elems);
        while $i < $n {
            my $sign := @elems[$i]<sign>;
            if $i > 0 && $sign eq '' {
                $sign."!clear_highwater"();
                $sign.panic('Missing + or - between character class elements')
            }
            @asts.push(@elems[$i].ast);
            $i++;
        }
        self.attach: $/, self.r('Regex', 'Assertion', 'CharClass').new(|@asts);
    }

    method cclass_elem($/) {
        my int $negated := $<sign> eq '-';
        if $<name> {
            self.attach: $/, self.r('Regex', 'CharClassElement', 'Rule').new:
                :name(~$<name>), :$negated;
        }
        elsif $<identifier> {
            my str $property := ~$<identifier>;
            my int $inverted := $<invert> eq '!';
            my $predicate := $<coloncircumfix>
                ?? $<coloncircumfix>.ast
                !! self.r('Expression');
            self.attach: $/, self.r('Regex', 'CharClassElement', 'Property').new:
                :$property, :$inverted, :$predicate, :$negated;
        }
        else {
            my @elements;
            for $<charspec> {
                if $_[1] {
                    my int $from := extract_endpoint($_[0]);
                    my int $to := extract_endpoint($_[1][0]);
                    my $node := self.r('Regex', 'CharClassEnumerationElement', 'Range');
                    @elements.push($node.new(:$from, :$to));
                }
                elsif $_[0]<cclass_backslash> {
                    @elements.push($_[0]<cclass_backslash>.ast);
                }
                else {
                    my $node := self.r('Regex', 'CharClassEnumerationElement', 'Character');
                    @elements.push($node.new(:character(~$_[0])));
                }
            }
            self.attach: $/, self.r('Regex', 'CharClassElement', 'Enumeration').new:
                :@elements, :$negated;
        }
    }

    sub extract_endpoint($/) {
        my str $chr;
        if $<cclass_backslash> {
            my $ast := $<cclass_backslash>.ast;
            if $ast.range-endpoint -> $ok {
                $chr := $ok;
            }
            else {
                $/.panic("Illegal range endpoint: " ~ ~$/)
            }
        }
        else {
            $chr := ~$/;
        }
        %*RX<m>
            ?? nqp::ordbaseat($chr, 0)
            !! non_synthetic_ord($/, $chr)
    }

    sub non_synthetic_ord($/, $chr) {
        my int $ord := nqp::ord($chr);
        if nqp::chr($ord) ne $chr {
            $/.panic("Cannot use $chr as a range endpoint, as it is not a single codepoint");
        }
        $ord
    }

    method cclass_backslash:sym<s>($/) {
        self.backslash:sym<s>($/)
    }

    method cclass_backslash:sym<b>($/) {
        self.attach: $/, self.r('Regex', 'CharClass', 'BackSpace').new(negated => $<sym> le 'Z');
    }

    method cclass_backslash:sym<e>($/) {
        self.backslash:sym<e>($/)
    }

    method cclass_backslash:sym<f>($/) {
        self.backslash:sym<f>($/)
    }

    method cclass_backslash:sym<h>($/) {
        self.backslash:sym<h>($/)
    }

    method cclass_backslash:sym<r>($/) {
        self.backslash:sym<r>($/)
    }

    method cclass_backslash:sym<t>($/) {
        self.backslash:sym<t>($/)
    }

    method cclass_backslash:sym<v>($/) {
        self.backslash:sym<v>($/)
    }

    method cclass_backslash:sym<o>($/) {
        self.backslash:sym<o>($/)
    }

    method cclass_backslash:sym<x>($/) {
        self.backslash:sym<x>($/)
    }

    method cclass_backslash:sym<c>($/) {
        self.backslash:sym<c>($/)
    }

    method cclass_backslash:sym<0>($/) {
        self.backslash:sym<0>($/)
    }

    method cclass_backslash:sym<any>($/) {
        self.attach: $/,
            self.r('Regex', 'CharClassEnumerationElement', 'Character').new(:character(~$/));
    }

    method mod_internal($/) {
        my constant NODE := nqp::hash('i', 'IgnoreCase', 'm', 'IgnoreMark',
            'r', 'Ratchet', 's', 'Sigspace');
        if NODE{$<mod_ident><sym>} -> $node-name {
            my str $n := $<n> ?? ~$<n>[0] !! '';
            my $negated := $n eq ''  ?? 0 !!
                           $n eq '!' ?? 1 !!
                           +$n == 0  ?? 1 !! 0;
            self.attach: $/, self.r('Regex', 'InternalModifier', $node-name).new(:$negated);
        }
        else {
            nqp::die('Unimplemented internal modifier ' ~ $<sym>);
        }
    }

    method codeblock($/) {
        make $<block>.ast;
    }

    method arglist($/) {
        make $<arglist>.ast;
    }
}
