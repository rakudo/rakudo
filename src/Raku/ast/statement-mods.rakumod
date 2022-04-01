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
class RakuAST::StatementModifier::If is RakuAST::StatementModifier::Condition {
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
class RakuAST::StatementModifier::Unless is RakuAST::StatementModifier::Condition {
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
class RakuAST::StatementModifier::When is RakuAST::StatementModifier::Condition {
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
class RakuAST::StatementModifier::With is RakuAST::StatementModifier::Condition {
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
class RakuAST::StatementModifier::Without is RakuAST::StatementModifier::Condition {
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
    method expression-thunk() { Nil }
}

# The while statement modifier.
class RakuAST::StatementModifier::While is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        if $sink {
            QAST::Op.new(
                :op('while'),
                self.expression.IMPL-TO-QAST($context),
                $statement-qast
            )
        }
        else {
            nqp::die('non-sink statement modifier while NYI');
        }
    }
}

# The until statement modifier.
class RakuAST::StatementModifier::Until is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        if $sink {
            QAST::Op.new(
                :op('until'),
                self.expression.IMPL-TO-QAST($context),
                $statement-qast
            )
        }
        else {
            nqp::die('non-sink statement modifier until NYI');
        }
    }
}

# The given statement modifier.
class RakuAST::StatementModifier::Given is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        self.IMPL-TEMPORARIZE-TOPIC(
            self.expression.IMPL-TO-QAST($context),
            $statement-qast
        )
    }
}

# The for statement modifier.
class RakuAST::StatementModifier::For is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        RakuAST::Statement::For.IMPL-FOR-QAST($context, 'serial',
            ($sink ?? 'sink' !! 'eager'),
            self.expression.IMPL-TO-QAST($context),
            $statement-qast)
    }

    method expression-thunk() {
        RakuAST::StatementModifier::For::Thunk.new
    }
}

# Thunk for the statement modifier for loop expression.
class RakuAST::StatementModifier::For::Thunk is RakuAST::ExpressionThunk {
    method IMPL-THUNK-SIGNATURE() {
        RakuAST::Signature.new(parameters => [
            RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$_'),
                traits => [
                    RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('raw'))
                ]
            )
        ])
    }
}
