# The base of all statement prefixes.
class RakuAST::StatementPrefix is RakuAST::Term {
    has RakuAST::Blorst $.blorst;

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!blorst);
    }
}

# The `do` statement prefix.
class RakuAST::StatementPrefix::Do is RakuAST::StatementPrefix is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $blorst := self.blorst;
        if nqp::istype($blorst, RakuAST::Block) {
            QAST::Op.new( :op('call'), $blorst.IMPL-TO-QAST($context) )
        }
        else {
            $blorst.IMPL-TO-QAST($context)
        }
    }
}
