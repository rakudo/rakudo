# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit is RakuAST::LexicalScope {
    has RakuAST::StatementList $.statement-list;
    has Str $.comp-unit-name;
    has Str $.setting-name;
    has Mu $!sc;

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

    method replace-statement-list(RakuAST::StatementList $statement-list) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!statement-list', $statement-list);
        Nil
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
