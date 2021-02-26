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

    method IMPL-TEMPORARIZE-TOPIC(Mu $new-topic-qast, Mu $with-topic-qast) {
        my $temporary := QAST::Node.unique('save_topic');
        QAST::Stmt.new(
            :resultchild(2),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($temporary), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('$_'), :scope('lexical') )
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('$_'), :scope('lexical') ),
                $new-topic-qast
            ),
            $with-topic-qast,
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('$_'), :scope('lexical') ),
                QAST::Var.new( :name($temporary), :scope('local') )
            )
        )
    }
}

# The base of all condition statement modifiers.
class RakuAST::StatementModifier::Condition is RakuAST::StatementModifier
                                            is RakuAST::ImplicitLookups {
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty'))
        ])
    }

    method IMPL-EMPTY(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        @lookups[0].IMPL-TO-QAST($context)
    }
}

# The if statement modifier.
class RakuAST::StatementModifier::Condition::If is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('if'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast,
            self.IMPL-EMPTY($context)
        )
    }
}

# The unless statement modifier.
class RakuAST::StatementModifier::Condition::Unless is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('unless'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast,
            self.IMPL-EMPTY($context)
        )
    }
}

# The when statement modifier.
class RakuAST::StatementModifier::Condition::When is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('callmethod'), :name('ACCEPTS'),
                self.expression.IMPL-TO-QAST($context),
                QAST::Var.new( :name('$_'), :scope('lexical') )
            ),
            $statement-qast,
            self.IMPL-EMPTY($context)
        )
    }
}

# The with statement modifier.
class RakuAST::StatementModifier::Condition::With is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        my $tested := QAST::Node.unique('with_tested');
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('callmethod'), :name('defined'),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($tested), :scope('local'), :decl('var') ),
                    self.expression.IMPL-TO-QAST($context),
                ),
            ),
            self.IMPL-TEMPORARIZE-TOPIC(
                QAST::Var.new( :name($tested), :scope('local') ),
                $statement-qast
            ),
            self.IMPL-EMPTY($context)
        )
    }
}

# The without statement modifier.
class RakuAST::StatementModifier::Condition::Without is RakuAST::StatementModifier::Condition {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        my $tested := QAST::Node.unique('without_tested');
        QAST::Op.new(
            :op('unless'),
            QAST::Op.new(
                :op('callmethod'), :name('defined'),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($tested), :scope('local'), :decl('var') ),
                    self.expression.IMPL-TO-QAST($context),
                ),
            ),
            self.IMPL-TEMPORARIZE-TOPIC(
                QAST::Var.new( :name($tested), :scope('local') ),
                $statement-qast
            ),
            self.IMPL-EMPTY($context)
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

# The given statement modifier.
class RakuAST::StatementModifier::Loop::Given is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        self.IMPL-TEMPORARIZE-TOPIC(
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}
