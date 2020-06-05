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

# A blockoid represents the block part of some kind of code declaration.
class RakuAST::Blockoid is RakuAST::Node {
    has RakuAST::StatementList $.statement-list;

    method new(RakuAST::StatementList $statement-list?) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Blockoid, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!statement-list.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!statement-list);
    }
}

# Marker for all code-y things.
class RakuAST::Code is RakuAST::Node {
}

# A block, either without signature or with only a placeholder signature.
class RakuAST::Block is RakuAST::LexicalScope is RakuAST::Term {
    has RakuAST::Blockoid $.body;

    method new(RakuAST::Blockoid :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Block, '$!body', $body // RakuAST::Blockoid.new);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        my $qb := QAST::Block.new(
            self.IMPL-QAST-DECLS($context),
            $!body.IMPL-TO-QAST($context)
        );
        if $immediate {
            $qb.blocktype('immediate');
            $qb
        }
        else {
            nqp::die('Non-immediate block compilation NYI');
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
    }
}

# A pointy block (-> $foo { ... }).
class RakuAST::PointyBlock is RakuAST::LexicalScope is RakuAST::Term is RakuAST::Meta
                           is RakuAST::Code {
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;

    method new(RakuAST::Signature :$signature!, RakuAST::Blockoid :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!signature', $signature);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!body', $body // RakuAST::Blockoid.new);
        $obj
    }

    method PRODUCE-META-OBJECT() {
        # Create block object and install signature.
        my $block := nqp::create(Block);
        nqp::bindattr($block, Code, '$!signature', $!signature.meta-object);
        $block
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself.
        my $block := QAST::Block.new(
            :blocktype('declaration_static'),
            self.IMPL-QAST-DECLS($context),
            $!signature.IMPL-TO-QAST($context),
            $!body.IMPL-TO-QAST($context)
        );

        # Obtain the meta-object and connect it to the code block.
        my $code-obj := self.meta-object;
        $context.ensure-sc($code-obj);
        $block.code_object($code-obj);

        # We need to do a fixup of the code block for the non-precompiled case.
        $context.add-fixup-task(-> {
            QAST::Op.new(
                :op('bindattr'),
                QAST::WVal.new( :value($code-obj) ),
                QAST::WVal.new( :value(Code) ),
                QAST::SVal.new( :value('$!do') ),
                QAST::BVal.new( :value($block) )
            )
        });

        $block
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $code-obj := self.meta-object;
        QAST::Op.new(
            :op('p6capturelex'),
            QAST::Op.new(
                :op('callmethod'), :name('clone'),
                QAST::WVal.new( :value($code-obj) )
            )
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        $visitor($!body);
    }
}

# Done by all kinds of Routine.
class RakuAST::Routine is RakuAST::LexicalScope is RakuAST::Term {
    has Bool $.is-multi;
    has Bool $.is-proto;
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;
}

# A subroutine.
class RakuAST::Sub is RakuAST::Routine {
}

# A method.
class RakuAST::Method is RakuAST::Routine {
}

# A submethod.
class RakuAST::Submethod is RakuAST::Routine {
}
