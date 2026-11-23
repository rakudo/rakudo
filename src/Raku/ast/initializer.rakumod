# All initializers do this marker. An initializer is the `= 42` part in a
# declaration like `my $a = 42`.
class RakuAST::Initializer
  is RakuAST::Node
{
    has RakuAST::ExpressionThunk $!thunk;

    method is-binding() { False }

    method IMPL-COMPILE-TIME-VALUE(RakuAST::Resolver $resolver,
        RakuAST::IMPL::QASTContext $context, Mu :$invocant-compiler)
    {
        RakuAST::Node.IMPL-BEGIN-TIME-EVALUATE(self.expression, $resolver, $context);
    }

    method IMPL-THUNK-EXPRESSION(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $expression := self.expression;
        my $thunk := Nil;
        unless (nqp::istype($expression, RakuAST::Code)) {
            $thunk := $expression.outer-most-thunk;
            unless ($thunk) {
                $thunk := RakuAST::ExpressionThunk.new;
                $expression.wrap-with-thunk($thunk);
                $thunk.IMPL-STUB-CODE($resolver, $context);
            }
            nqp::bindattr(self, RakuAST::Initializer, '$!thunk', $thunk);
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $expression := self.expression;
        nqp::istype($expression, RakuAST::Code)
            ?? $expression.IMPL-TO-QAST($context)
            !! $!thunk.IMPL-QAST-BLOCK($context, :expression($expression))
    }
}

# Role for classes that initialize from an expression
class RakuAST::Initializer::Expression
  is RakuAST::Initializer
{
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        $obj.set-expression($expression);
        $obj
    }

    method set-expression(RakuAST::Expression $expression) {
        nqp::bindattr(self, RakuAST::Initializer::Expression, '$!expression',
          $expression // RakuAST::Expression);
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }

    method IMPL-TO-QAST(
      RakuAST::IMPL::QASTContext $context, Mu :$invocant-qast
    ) {
        $!expression.IMPL-TO-QAST($context)
    }
}

# An assignment (`=`) initializer.
class RakuAST::Initializer::Assign
  is RakuAST::Initializer::Expression
{
    method is-binding() { False }
}

# A bind (`:=`) initializer.
class RakuAST::Initializer::Bind
  is RakuAST::Initializer::Expression
{
    method is-binding() { True }
}

# An mutating method call (`.=`) initializer.
class RakuAST::Initializer::CallAssign
  is RakuAST::Initializer
{
    has RakuAST::Postfixish $.postfixish;

    method new(RakuAST::Postfixish $postfixish) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Initializer::CallAssign, '$!postfixish', $postfixish);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!postfixish);
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!postfixish.add-colonpair($pair);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, Mu :$invocant-qast) {
        $!postfixish.IMPL-POSTFIX-QAST($context, $invocant-qast)
    }

    method IMPL-COMPILE-TIME-VALUE(RakuAST::Resolver $resolver,
        RakuAST::IMPL::QASTContext $context, Mu :$invocant-compiler)
    {
        self.postfixish.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new(:$resolver, :$context), $invocant-compiler)
    }

    method IMPL-THUNK-EXPRESSION(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<null>)
    }
}
