# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit is RakuAST::LexicalScope is RakuAST::SinkBoundary {
    has RakuAST::StatementList $.statement-list;
    has Str $.comp-unit-name;
    has Str $.setting-name;
    has Mu $!sc;
    has int $!is-sunk;

    method new(RakuAST::StatementList :$statement-list, Str :$comp-unit-name!,
            Str :$setting-name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!comp-unit-name', $comp-unit-name);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!setting-name', $setting-name // Str);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!sc', nqp::createsc($comp-unit-name));
        $obj
    }

    # Replace the statement list of the compilation unit.
    method replace-statement-list(RakuAST::StatementList $statement-list) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!statement-list', $statement-list);
        Nil
    }

    # Indicate that this compilation unit is sunk as a whole (that, is, the result of
    # its last statement is unused).
    method mark-sunk() {
        nqp::bindattr_i(self, RakuAST::CompUnit, '$!is-sunk', 1);
        Nil
    }

    method is-boundary-sunk() {
        $!is-sunk ?? True !! False
    }

    method get-boundary-sink-propagator() {
        $!statement-list
    }

    method IMPL-TO-QAST-COMP-UNIT(*%options) {
        # Create compilation context.
        nqp::pushcompsc($!sc);
        my $context := RakuAST::IMPL::QASTContext.new(:sc($!sc));
        my $top-level := QAST::Block.new;
        self.IMPL-ADD-SETTING-LOADING($context, $top-level, $!setting-name) if $!setting-name;

        # Compile into a QAST::CompUnit.
        $top-level.push(self.IMPL-TO-QAST($context));
        QAST::CompUnit.new: $top-level, :hll('Raku'), :sc($!sc),
            :post_deserialize($context.post-deserialize()),
    }

    method IMPL-ADD-SETTING-LOADING(RakuAST::IMPL::QASTContext $context, Mu $top-level, Str $name) {
        # Use the NQP module loader to load Perl6::ModuleLoader, which
        # is a normal NQP module.
        my $module-loading := QAST::Stmt.new(
            QAST::Op.new(
                :op('loadbytecode'),
                QAST::VM.new(
                    :jvm(QAST::SVal.new( :value('ModuleLoader.class') )),
                    :moar(QAST::SVal.new( :value('ModuleLoader.moarvm') )),
                    :js(QAST::SVal.new( :value('ModuleLoader') ))
                )),
            QAST::Op.new(
                :op('callmethod'), :name('load_module'),
                QAST::Op.new(
                    :op('gethllsym'),
                    QAST::SVal.new( :value('nqp') ),
                    QAST::SVal.new( :value('ModuleLoader') )
                ),
                QAST::SVal.new( :value('Perl6::ModuleLoader') )
            ));

        # Load and put in place the setting after deserialization and on fixup.
        $context.add-fixup-and-deserialize-task(QAST::Stmt.new(
            $module-loading,
            QAST::Op.new(
                :op('forceouterctx'),
                QAST::BVal.new( :value($top-level) ),
                QAST::Op.new(
                    :op('callmethod'), :name('load_setting'),
                    QAST::Op.new(
                        :op('getcurhllsym'),
                        QAST::SVal.new( :value('ModuleLoader') )
                    ),
                    QAST::SVal.new( :value($name) )
                )
            )));
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Stmts.new(
            QAST::Var.new( :name('__args__'), :scope('local'), :decl('param'), :slurpy(1) ),
            self.IMPL-QAST-DECLS($context),
            $!statement-list.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!statement-list);
    }
}

# Builds and interns numeric and string literals.
class RakuAST::LiteralBuilder {
    # Build an Int constant and intern it.
    method intern-int(str $chars, int $base, Mu $error-reporter?) {
        # TODO interning
        self.build-int($chars, $base, $error-reporter)
    }

    # Build an Int constant, but do not intern it.
    method build-int(str $source, int $base, Mu $error-reporter?) {
        my $res := nqp::radix_I($base, $source, 0, 2, Int);
        unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), nqp::chars($source)) {
            $error-reporter ??
                $error-reporter()
                !! nqp::die("'$source' is not a valid number");
        }
        nqp::atpos($res, 0)
    }

    # Build a Str constant and intern it.
    method intern-str(str $chars) {
        # TODO interning
        self.build-str($chars)
    }

    # Build an Str constant, but do not intern it.
    method build-str(str $chars) {
        nqp::box_s($chars, Str)
    }
}
