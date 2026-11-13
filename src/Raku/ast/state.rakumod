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

# A state initializer runs on its first reach in each clone of the frame
# declaring the variable, as once does. A sentinel state variable guards
# it, produced as an implicit declaration for the declaring scope to hold.
role RakuAST::StateInitGuard {
    has RakuAST::VarDeclaration::Implicit::State $!state-init-guard;

    # Whether the declaration has a state initializer to guard. A binding
    # one keeps nqp::p6stateinit, as the binder writes into its own frame.
    method IMPL-GUARDS-STATE-INIT() {
        self.scope eq 'state' && nqp::isconcrete(self.initializer)
          && !self.initializer.is-binding
    }

    method IMPL-STATE-INIT-GUARD() {
        $!state-init-guard // nqp::bindattr(self, RakuAST::StateInitGuard,
            '$!state-init-guard', RakuAST::VarDeclaration::Implicit::State.new(
                QAST::Node.unique('!state_init_guard'), :sentinel))
    }

    # Whether the initializer is to run, marking the guard on the first reach.
    # A sub in a role body forms its code before its scope gathers the
    # implicit declarations, so the guard is produced here as well.
    method IMPL-STATE-INIT-CONDITION-QAST(RakuAST::IMPL::QASTContext $context) {
        return QAST::Op.new( :op('p6stateinit') ) unless self.IMPL-GUARDS-STATE-INIT;
        self.get-implicit-declarations;
        my $guard := self.IMPL-STATE-INIT-GUARD;
        QAST::Op.new(:op<if>,
          $guard.IMPL-SENTINEL-TEST-QAST($context),
          QAST::Stmts.new(
            QAST::Op.new(:op<p6store>,
              QAST::Var.new( :name($guard.name), :scope<lexical> ),
              QAST::WVal.new( :value(True) )
            ),
            QAST::IVal.new( :value(1) )
          ),
          QAST::IVal.new( :value(0) )
        )
    }
}
