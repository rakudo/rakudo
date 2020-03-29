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
class RakuAST::PointyBlock is RakuAST::LexicalScope is RakuAST::Term {
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;
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
