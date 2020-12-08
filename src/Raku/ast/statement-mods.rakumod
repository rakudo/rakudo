# The base of all statement modifiers.
class RakuAST::StatementModifier is RakuAST::Node {
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementModifier, '$!expression', $expression);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }
}

# The base of all condition statement modifiers.
class RakuAST::StatementModifier::Condition is RakuAST::StatementModifier {
}

# The if statement modifier.
class RakuAST::StatementModifier::Condition::If is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('if'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}

# The unless statement modifier.
class RakuAST::StatementModifier::Condition::Unless is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('unless'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}

# The base of all loop statement modifiers.
class RakuAST::StatementModifier::Loop is RakuAST::StatementModifier {
}

# The while statement modifier.
class RakuAST::StatementModifier::Loop::While is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('while'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}

# The until statement modifier.
class RakuAST::StatementModifier::Loop::Until is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('until'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}
