# The base of all statement modifiers.
class RakuAST::StatementModifier
  is RakuAST::Node
{
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        $obj.set-expression($expression);
        $obj
    }

    method set-expression(RakuAST::Expression $expression) {
        nqp::bindattr(self, RakuAST::StatementModifier, '$!expression',
            $expression // RakuAST::Expression);
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }
}

# The base of all condition statement modifiers.
class RakuAST::StatementModifier::Condition
  is RakuAST::StatementModifier
  is RakuAST::ImplicitLookups
{
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty'))
        ])
    }

    method IMPL-EMPTY(RakuAST::IMPL::QASTContext $context) {
        self.get-implicit-lookups.AT-POS(0).IMPL-TO-QAST($context)
    }

    method expression-thunk() {
        RakuAST::StatementModifier::Condition::Thunk.new(self)
    }
}

# The if statement modifier.
class RakuAST::StatementModifier::If
  is RakuAST::StatementModifier::Condition
{
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
class RakuAST::StatementModifier::Unless
  is RakuAST::StatementModifier::Condition
{
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
class RakuAST::StatementModifier::When
  is RakuAST::StatementModifier::Condition
{
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
class RakuAST::StatementModifier::With
  is RakuAST::StatementModifier::Condition
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        if nqp::istype($statement-qast, QAST::Block) {
            # It's a block, so just use the `with` compilation.
            QAST::Op.new(
                :op('with'),
                self.expression.IMPL-TO-QAST($context),
                $statement-qast,
                self.IMPL-EMPTY($context)
            )
        }
        else {
            # A non-block statement. Compile more cheaply by making a temporary
            # $_ to avoid a wrapping block.
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
}

# The without statement modifier.
class RakuAST::StatementModifier::Without
  is RakuAST::StatementModifier::Condition
{
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
class RakuAST::StatementModifier::Loop
  is RakuAST::StatementModifier
{
    method expression-thunk() { Nil }
}

# The while statement modifier.
class RakuAST::StatementModifier::While
  is RakuAST::StatementModifier::Loop
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block) {
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
class RakuAST::StatementModifier::Until
  is RakuAST::StatementModifier::Loop
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block) {
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
class RakuAST::StatementModifier::Given
  is RakuAST::StatementModifier::Loop
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block) {
        if $block {
            if $sink {
                $statement-qast[0].push(self.expression.IMPL-TO-QAST($context));
            }
            else {
                $statement-qast.push(self.expression.IMPL-TO-QAST($context));
            }
            $statement-qast
        }
        else {
            self.IMPL-TEMPORARIZE-TOPIC(
                self.expression.IMPL-TO-QAST($context),
                $statement-qast
            )
        }
    }
}

# The for statement modifier.
class RakuAST::StatementModifier::For
  is RakuAST::StatementModifier::Loop
  is RakuAST::ForLoopImplementation
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block) {
        my $expression := self.expression;
        my $expression-qast := $expression.IMPL-TO-QAST($context);
        $statement-qast := $sink ?? $statement-qast[0][0] !! $statement-qast[0] if $block;

        nqp::istype($expression, RakuAST::QuotedRegex)
                        ??
            self.IMPL-TEMPORARIZE-TOPIC(
                $expression-qast,
                self.IMPL-FOR-QAST(
                    $context, 'serial',
                    ($sink ?? 'sink' !! 'eager'),
                    $expression-qast,
                    $statement-qast))
                        !!
             self.IMPL-FOR-QAST(
                $context, 'serial',
                ($sink ?? 'sink' !! 'eager'),
                $expression-qast,
                $statement-qast)
    }

    method expression-thunk() {
        RakuAST::StatementModifier::For::Thunk.new
    }
}

# Thunk for the statement modifier for loop expression.
class RakuAST::StatementModifier::For::Thunk
  is RakuAST::ExpressionThunk
{
    method IMPL-THUNK-SIGNATURE() {
        RakuAST::Signature.new(parameters => [
            RakuAST::Parameter.new(
              target => RakuAST::ParameterTarget::Var.new(:name<$_>),
              default-raw => True,
          )
        ])
    }
}

class RakuAST::StatementModifier::Condition::Thunk
  is RakuAST::ExpressionThunk
{
    has RakuAST::StatementModifier::Condition $!condition;

    method new(RakuAST::StatementModifier::Condition $condition) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementModifier::Condition::Thunk, '$!condition', $condition);
        $obj
    }

    method IMPL-THUNK-CODE-QAST(RakuAST::IMPL::QASTContext $context, Mu $target,
            RakuAST::Expression $expression) {

        #TODO handle inner thunks
        $target.push($!condition.IMPL-WRAP-QAST($context, $expression.IMPL-EXPR-QAST($context)));
    }

    method IMPL-THUNK-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method PRODUCE-META-OBJECT() {
        Nil
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context,
            str :$blocktype, RakuAST::Expression :$expression!) {
        nqp::die('must not call this ' ~ $!condition.dump);
    }
}
