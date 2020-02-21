# A blockoid represents the block part of some kind of code declaration.
class RakuAST::Blockoid is RakuAST::Node {
    has RakuAST::StatementList $.statement-list;

    method new(RakuAST::StatementList $statement-list) {
        my $obj := nqp::create(self);
        nqp::bindattr(self, RakuAST::Blockoid, '$!statement-list', $statement-list);
        $obj
    }
}

# A pointy block (-> $foo { ... }).
class RakuAST::PointyBlock is RakuAST::Node {
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;
}

# Done by all kinds of Routine.
class RakuAST::Routine is RakuAST::Node {
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
