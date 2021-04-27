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
}

# The while statement modifier.
class RakuAST::StatementModifier::While is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        if $sink {
            nqp::die('non-sink statement modifier until NYI');
        }
        else {
            QAST::Op.new(
                :op('while'),
                self.expression.IMPL-TO-QAST($context),
                $statement-qast
            )
        }
    }
}

# The until statement modifier.
class RakuAST::StatementModifier::Until is RakuAST::StatementModifier::Loop {
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        if $sink {
            nqp::die('non-sink statement modifier until NYI');
        }
        else {
            QAST::Op.new(
                :op('until'),
                self.expression.IMPL-TO-QAST($context),
                $statement-qast
            )
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
class RakuAST::StatementModifier::For is RakuAST::StatementModifier::Loop
                                      is RakuAST::Meta is RakuAST::Code {
    # The block that we create to hold the thunked expression; we create it
    # when processing declarative things, and fill it out later when we are
    # passed the imperative code.
    has Mu $!qast-block;

    method PRODUCE-META-OBJECT() {
        # Create code object taking a parameter `$_ is raw`
        my constant SIG_ELEM_IS_RAW := 1024;
        my $param := nqp::create(Parameter);
        nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
        nqp::bindattr($param, Parameter, '$!type', Mu);
        nqp::bindattr_i($param, Parameter, '$!flags', SIG_ELEM_IS_RAW);
        my $signature := nqp::create(Signature);
        nqp::bindattr($signature, Signature, '@!params', nqp::list($param));
        nqp::bindattr_i($signature, Signature, '$!arity', 1);
        nqp::bindattr($signature, Signature, '$!count', nqp::box_i(1, Int));
        my $code := nqp::create(Code);
        nqp::bindattr($code, Code, '$!signature', $signature);
        $code
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        my $block := QAST::Block.new(
            :blocktype('declaration_static'),
            QAST::Var.new( :name('$_'), :scope('lexical'), :decl('param') )
        );
        self.IMPL-LINK-META-OBJECT($context, $block);
        nqp::bindattr(self, RakuAST::StatementModifier::For, '$!qast-block', $block);
        $block
    }

    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink) {
        # Place the code we get into the block we produced.
        my $block := nqp::ifnull($!qast-block,
            nqp::die('Missing QAST block in `for` statement modifier'));
        $block.push($statement-qast);

        # Delegate compilation to the non-modifier `for` statement.
        RakuAST::Statement::For.IMPL-FOR-QAST($context, 'serial',
            ($sink ?? 'sink' !! 'eager'),
            self.expression.IMPL-TO-QAST($context),
            self.IMPL-CLOSURE-QAST($context))
    }
}
