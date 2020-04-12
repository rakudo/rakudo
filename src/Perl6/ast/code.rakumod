# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit is RakuAST::LexicalScope {
    has RakuAST::StatementList $.statement-list;

    method new(RakuAST::StatementList $statement-list?) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Stmts.new(
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
                           is RakuAST::ImplicitLookups is RakuAST::Code {
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;

    method new(RakuAST::Signature :$signature!, RakuAST::Blockoid :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!signature', $signature);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!body', $body // RakuAST::Blockoid.new);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Simple.new('Code'),
            RakuAST::Type::Simple.new('Block'),
        ])
    }

    method PRODUCE-META-OBJECT() {
        # Create block object and install signature.
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $code-type := @lookups[0].resolution.compile-time-value;
        my $block-type := @lookups[1].resolution.compile-time-value;
        my $block := nqp::create($block-type);
        nqp::bindattr($block, $code-type, '$!signature', $!signature.meta-object);
        $block
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself.
        my $block := QAST::Block.new(
            :blocktype('declaration_static'),
            self.IMPL-QAST-DECLS($context),
            $!body.IMPL-TO-QAST($context)
        );

        # Obtain the meta-object and connect it to the code block.
        my $code-obj := self.meta-object;
        $context.ensure-sc($code-obj);
        $block.code_object($code-obj);

        # We need to do a fixup of the code block for the non-precompiled case.
        $context.add-fixup-task(-> {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
            my $code-type := @lookups[0].resolution.compile-time-value;
            QAST::Op.new(
                :op('bindattr'),
                QAST::WVal.new( :value($code-obj) ),
                QAST::WVal.new( :value($code-type) ),
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
