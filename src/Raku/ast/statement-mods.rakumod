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
    # Set by the optimize pass, allowing a native-int condition to be
    # tested directly.
    has int $!native-condition;

    method IMPL-SET-NATIVE-CONDITION() {
        nqp::bindattr_i(self, RakuAST::StatementModifier::Condition, '$!native-condition', 1)
    }

    method IMPL-NATIVE-CONDITION() { $!native-condition }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty'))
        ]
    }

    method IMPL-EMPTY(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
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
        my $cond-qast := self.expression.IMPL-TO-QAST($context);
        $cond-qast := self.IMPL-NATIVE-CONDITION-QAST($cond-qast)
            if self.IMPL-NATIVE-CONDITION;
        QAST::Op.new(
            :op('if'),
            $cond-qast,
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
        my $cond-qast := self.expression.IMPL-TO-QAST($context);
        $cond-qast := self.IMPL-NATIVE-CONDITION-QAST($cond-qast)
            if self.IMPL-NATIVE-CONDITION;
        QAST::Op.new(
            :op('unless'),
            $cond-qast,
            $statement-qast,
            self.IMPL-EMPTY($context)
        )
    }
}

# The when statement modifier.
class RakuAST::StatementModifier::When
  is RakuAST::StatementModifier::Condition
{
    # Set by the optimize pass when the matcher reduces to a type check:
    # the type matched against, and the Junction type for the runtime
    # topic guard, or null when the matcher is Junction itself.
    has int $!typematch;
    has Mu $!typematch-type;
    has Mu $!typematch-junction;

    method IMPL-SET-TYPEMATCH(Mu $type, Mu $junction) {
        nqp::bindattr_i(self, RakuAST::StatementModifier::When, '$!typematch', 1);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!typematch-type', $type);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!typematch-junction', $junction);
    }

    # Set by the optimize pass when the matcher reduces to a literal
    # comparison against the topic.
    has int $!litmatch;
    has Mu $!litmatch-data;

    method IMPL-SET-LITMATCH(Mu $data) {
        nqp::bindattr_i(self, RakuAST::StatementModifier::When, '$!litmatch', 1);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!litmatch-data', $data);
    }

    # Set by the optimize pass when the matcher is a compile-time Pair
    # that reduces to asking the topic the method its key names.
    has int $!pairmatch;
    has Mu $!pairmatch-data;

    method IMPL-SET-PAIRMATCH(Mu $data) {
        nqp::bindattr_i(self, RakuAST::StatementModifier::When, '$!pairmatch', 1);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!pairmatch-data', $data);
    }

    # Set by the optimize pass when the matcher is a junction of types
    # that reduces to a chain of type checks on the topic.
    has int $!juncmatch;
    has Mu $!juncmatch-data;
    has Mu $!juncmatch-junction;

    method IMPL-SET-JUNCTION-TYPEMATCH(Mu $data, Mu $junction) {
        nqp::bindattr_i(self, RakuAST::StatementModifier::When, '$!juncmatch', 1);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!juncmatch-data', $data);
        nqp::bindattr(self, RakuAST::StatementModifier::When, '$!juncmatch-junction', $junction);
    }

    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        QAST::Op.new(
            :op('if'),
            $!typematch
                ?? self.IMPL-WHEN-TYPEMATCH-QAST($context,
                    QAST::Var.new( :name('$_'), :scope('lexical') ),
                    $!typematch-type, $!typematch-junction)
                !! $!litmatch
                    ?? self.IMPL-LITMATCH-QAST($context,
                        QAST::Var.new( :name('$_'), :scope('lexical') ),
                        $!litmatch-data, 0)
                    !! $!pairmatch
                        ?? self.IMPL-PAIRMATCH-QAST($context,
                            QAST::Var.new( :name('$_'), :scope('lexical') ),
                            $!pairmatch-data, 0)
                        !! $!juncmatch
                            ?? self.IMPL-JUNCTION-TYPEMATCH-QAST($context,
                                QAST::Var.new( :name('$_'), :scope('lexical') ),
                                $!juncmatch-data, $!juncmatch-junction, 0)
                            !! QAST::Op.new(
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
                    :op('not_i'),
                    QAST::Op.new(
                        :op('isnull'),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($tested), :scope('local'), :decl('var') ),
                            self.expression.IMPL-TO-QAST($context),
                        )
                    )
                ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('callmethod'), :name('defined'),
                        QAST::Var.new( :name($tested), :scope('local') ),
                    ),
                    self.IMPL-TEMPORARIZE-TOPIC(
                        QAST::Var.new( :name($tested), :scope('local') ),
                        $statement-qast
                    ),
                    self.IMPL-EMPTY($context)
                )
            )
        }
    }
}

# The without statement modifier.
class RakuAST::StatementModifier::Without
  is RakuAST::StatementModifier::Condition
{
    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast) {
        if nqp::istype($statement-qast, QAST::Block && $statement-qast.code_object.count) {
            my $code-obj := $statement-qast.code_object;
            $context.ensure-sc($code-obj);
            my $clone := QAST::Op.new(
                :op('callmethod'), :name('clone'),
                QAST::WVal.new( :value($code-obj) ).annotate_self('past_block', $statement-qast).annotate_self('code_object', $code-obj)
            );
            my $closure := QAST::Op.new( :op('p6capturelex'), $clone );

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
                QAST::Op.new(
                    :op('call'),
                    $closure,
                    QAST::Var.new( :name($tested), :scope('local') ),
                ),
                self.IMPL-EMPTY($context)
            )
        }
        else {
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
}

# The base of all loop statement modifiers.
class RakuAST::StatementModifier::Loop
  is RakuAST::StatementModifier
{
    method expression-thunk() { Nil }

    method handles-condition() { True }
}

class RakuAST::StatementModifier::WhileUntil
  is RakuAST::StatementModifier::Loop
  is RakuAST::ImplicitLookups
{
    # Is the condition negated?
    method negate() { False }

    method handles-condition() { False }

    method IMPL-NEGATE-IF-NEEDED(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
    }

    method IMPL-UNNEGATE-IF-NEEDED() {
        Nil
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Seq'))
        ]
    }

    # Set by the optimize pass, allowing a native-int condition to be
    # tested directly.
    has int $!native-condition;

    method IMPL-SET-NATIVE-CONDITION() {
        nqp::bindattr_i(self, RakuAST::StatementModifier::WhileUntil, '$!native-condition', 1)
    }

    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block, Mu :$expression) {
        if $sink {
            my $cond-qast := self.expression.IMPL-TO-QAST($context);
            $cond-qast := self.IMPL-NATIVE-CONDITION-QAST($cond-qast)
                if $!native-condition;
            QAST::Op.new(
                :op(self.negate ?? 'until' !! 'while'),
                $cond-qast,
                $statement-qast
            )
        }
        else {
            my $Seq := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].IMPL-TO-QAST($context);
            QAST::Op.new(:op<callmethod>, :name('from-loop'),
                $Seq,
                $statement-qast,
                self.expression.IMPL-TO-QAST($context),
            )
        }
    }
}

# The while statement modifier.
class RakuAST::StatementModifier::While
  is RakuAST::StatementModifier::WhileUntil
{
}

# The until statement modifier.
class RakuAST::StatementModifier::Until
  is RakuAST::StatementModifier::WhileUntil
{
    method negate() { True }

    method IMPL-NEGATE-IF-NEEDED(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr(self, RakuAST::StatementModifier, '$!expression', RakuAST::ApplyPostfix.new(
            :postfix(
                RakuAST::Call::Method.new(:name(RakuAST::Name.from-identifier('not')))
            ),
            :operand(self.expression),
        ));
        self.expression.ensure-begin-performed($resolver, $context);
    }

    method IMPL-UNNEGATE-IF-NEEDED() {
        nqp::bindattr(self, RakuAST::StatementModifier, '$!expression', self.expression.operand);
        True
    }
}

# The given statement modifier.
class RakuAST::StatementModifier::Given
  is RakuAST::StatementModifier::Loop
{
    method handles-condition() { False }

    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block, Mu :$expression) {
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
  is RakuAST::ImplicitLookups
{
    # Set when the optimize pass has approved lowering a CORE integer-range
    # source to a native counting loop.
    has int $!can-lower-range;

    method IMPL-SET-CAN-LOWER-RANGE() {
        nqp::bindattr_i(self, RakuAST::StatementModifier::For, '$!can-lower-range', 1)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical::Setting.new(
                :desigilname(RakuAST::Name.from-identifier('IterationEnd'))),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ]
    }

    method IMPL-WRAP-QAST(RakuAST::IMPL::QASTContext $context, Mu $statement-qast, Bool :$sink, Bool :$block, Mu :$expression) {
        my $source := self.expression;
        my $source-qast := $source.IMPL-TO-QAST($context);
        $statement-qast := $sink ?? $statement-qast[0][0] !! $statement-qast[0] if $block;

        # A sunk modifier for loop with a simple enough body iterates its
        # source directly rather than delegating to its map method. A
        # thunked statement always takes the one topic argument; an
        # explicit block is checked like the statement form.
        my $for-qast;
        if $sink && ($block
            ?? nqp::istype($expression, RakuAST::Code)
                 && self.IMPL-CAN-USE-STATEMENT-FORM($expression)
            !! True) {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $Nil := @lookups[1].resolution.compile-time-value;

            # An integer-range source the optimize pass approved becomes a
            # native counting loop, unless a bound turns out not to be a
            # native-friendly integer.
            if $!can-lower-range
                && !($block && nqp::istype($expression, RakuAST::Code)
                      && $expression.has-any-phasers) {
                $for-qast := self.IMPL-TO-QAST-RANGE(
                    $context, $source, $statement-qast, $Nil);
            }

            if !nqp::isconcrete($for-qast) {
                $for-qast := self.IMPL-TO-QAST-STATEMENT(
                    $context,
                    $source-qast,
                    $statement-qast,
                    RakuAST::Label,
                    @lookups[0].resolution.compile-time-value,
                    $Nil
                );
            }
        }
        else {
            $for-qast := self.IMPL-FOR-QAST(
                $context, 'serial',
                ($sink ?? 'sink' !! 'eager'),
                $source-qast,
                $statement-qast
            );
        }

        nqp::istype($source, RakuAST::QuotedRegex)
            ?? self.IMPL-TEMPORARIZE-TOPIC($source-qast, $for-qast)
            !! $for-qast
    }

    method expression-thunk() {
        RakuAST::StatementModifier::For::Thunk.new
    }
}

# Thunk for the statement modifier for loop expression.
class RakuAST::StatementModifier::For::Thunk
  is RakuAST::ExpressionThunk
{
    method declare-topic() {
        True
    }

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

        # Statement::Expression wraps this thunk before any loop thunk, so it
        # is always innermost and the expression can be emitted directly. A
        # caller that chained it over another thunk would silently lose that
        # thunk, so refuse it.
        nqp::die('Condition modifier thunk cannot wrap an inner thunk')
            if self.next;
        $target.push($!condition.IMPL-WRAP-QAST($context, $expression.IMPL-EXPR-QAST($context)));
    }

    method IMPL-THUNK-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        Nil
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context,
            str :$blocktype, RakuAST::Expression :$expression!) {
        nqp::die('must not call this ' ~ $!condition.dump);
    }
}
