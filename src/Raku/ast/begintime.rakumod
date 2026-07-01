# Done by all things that want to perform some kind of effect at BEGIN time.
# They may do that effect before their children are visited in resolution or
# after; the default is after.
class RakuAST::BeginTime
  is RakuAST::Node
{
    has int $!begin-performed;

    # Method implemented by a node to perform its begin-time side-effects.
    # If a class does not implement it, then this will fire
    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::die('Missing PERFORM-BEGIN implementation in ' ~ self.HOW.name(self));
    }

    # Ensure the begin-time effects are performed.
    method ensure-begin-performed(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        unless $!begin-performed {
            self.PERFORM-BEGIN($resolver, $context);
            nqp::bindattr_i(self, RakuAST::BeginTime, '$!begin-performed', 1);
        }
        Nil
    }

    # Called when a BEGIN-time construct needs to evaluate code. Tries to
    # interpret simple things to avoid the cost of compilation.
    method IMPL-BEGIN-TIME-EVALUATE(
                   RakuAST::Node $code,
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $*IMPL-COMPILE-DYNAMICALLY := 1;

        # Handle any execution error appropriately
        CATCH {
            my $ex := $resolver.convert-exception($_);

            # Can handle it properly
            if nqp::istype(self,RakuAST::CheckTime) {
                self.add-sorry: $ex;
                $resolver.note-deferred-begin-sorry;
            }

            # Alas, need to rethrow wil line info if possible
            else {
                if nqp::can($ex,'SET_FILE_LINE')
                  && self
                  && my $origin := self.origin {
                    my $origin-match := $origin.as-match;
                    $ex.SET_FILE_LINE($origin-match.file, $origin-match.line);
                }
                $ex.rethrow;
            }
        }

        # Can interprete, so do that
        if $code.IMPL-CAN-INTERPRET {
            $code.IMPL-INTERPRET(
              RakuAST::IMPL::InterpContext.new(:$resolver, :$context)
            )
        }

        # A deferred phaser like INIT has not run at BEGIN time, so its value
        # is whatever its cache holds, which is undefined until it runs. Hand
        # that back rather than the block itself.
        elsif nqp::istype($code, RakuAST::StatementPrefix::Phaser::Init) {
            nqp::ifnull(nqp::decont($code.container), Mu)
        }

        # It's already code
        elsif nqp::istype($code, RakuAST::Code) {
            $code.meta-object
        }

        # Wrap an expression in a thunk
        elsif nqp::istype($code, RakuAST::Expression) {
            my $thunk := RakuAST::ExpressionThunk.new;
            $code.wrap-with-thunk($thunk);
            $thunk.IMPL-STUB-CODE($resolver, $context);
            $code.apply-sink(False);
            $thunk.IMPL-QAST-BLOCK($context, :expression($code));
            $thunk.meta-object()()
        }
        else {
            nqp::die('BEGIN time evaluation only supported for simple constructs so far')
        }
    }

    # Called when a BEGIN-time construct wants to evaluate a resolved code
    # with a set of arguments.
    method IMPL-BEGIN-TIME-CALL(
                   RakuAST::Node $callee,
                RakuAST::ArgList $args,
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $*IMPL-COMPILE-DYNAMICALLY := 1;

        # Ready to call
        if $callee.is-resolved
          && nqp::istype($callee.resolution, RakuAST::CompileTimeValue)
          && $args.IMPL-CAN-INTERPRET {
            my $resolved := $callee.resolution.compile-time-value;
            my @args := $args.IMPL-INTERPRET(
              RakuAST::IMPL::InterpContext.new(:$resolver, :$context)
            );

            # Separate out positional/named args first before flattening
            # them, as NQP is not as smart as is sometimes expected
            my @pos   := @args[0];
            my %named := @args[1];
            $resolved(|@pos, |%named)
        }

        # Not ready, wrap in a call and evaluate that
        else {
            my $call := RakuAST::ApplyPostfix.new(
              :postfix(RakuAST::Call::Term.new(:$args)),
              :operand($callee)
            );
            $call.to-begin-time($resolver, $context);
            self.IMPL-BEGIN-TIME-EVALUATE($call, $resolver, $context)
        }
    }
}
