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

    # What a pseudo-stash lookup asks once the context chain it walks runs
    # out. Code run for a begin-time effect has the setting as its outer,
    # so that chain reaches setting symbols and stops short of what the
    # unit being compiled declares. The token restricts the answer to
    # frames this compilation produced: the lookup consults the resolver
    # only when the frame it runs in sees the same token, so foreign code
    # the effect merely runs is not answered for. Null without a context,
    # since no frame can then carry the token.
    method IMPL-BEGIN-TIME-LOOKUP-STATE(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::isconcrete($context)
          ?? nqp::list($resolver, $context.begin-time-marker)
          !! nqp::null()
    }

    # Ensure the begin-time effects are performed.
    method ensure-begin-performed(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        unless $!begin-performed {
            my $*BEGIN-TIME-LOOKUP :=
              RakuAST::BeginTime.IMPL-BEGIN-TIME-LOOKUP-STATE($resolver, $context);
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
        my $*BEGIN-TIME-LOOKUP :=
          RakuAST::BeginTime.IMPL-BEGIN-TIME-LOOKUP-STATE($resolver, $context);

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
        my $result;
        if $code.IMPL-CAN-INTERPRET {
            $result := $code.IMPL-INTERPRET(
              RakuAST::IMPL::InterpContext.new(:$resolver, :$context)
            )
        }

        # A deferred phaser like INIT has not run at BEGIN time, so its value
        # is whatever its cache holds, which is undefined until it runs. Hand
        # that back rather than the block itself.
        elsif nqp::istype($code, RakuAST::StatementPrefix::Phaser::Init) {
            $result := nqp::ifnull(nqp::decont($code.container), Mu)
        }

        # A code literal's begin-time value is its own code object,
        elsif nqp::istype($code, RakuAST::Code)
          # unless it is a value-producing statement prefix (gather, start, and
          # friends), whose value is what running it produces, not the code,
          && (!nqp::istype($code, RakuAST::StatementPrefix)
              # though a phaser is a statement prefix whose caller runs the
              # code object we hand back, so keep those.
              || nqp::istype($code, RakuAST::StatementPrefix::Phaser)) {
            $result := $code.meta-object
        }

        # Wrap an expression in a thunk
        elsif nqp::istype($code, RakuAST::Expression) {
            my $thunk := RakuAST::ExpressionThunk.new;
            $code.wrap-with-thunk($thunk);
            $thunk.IMPL-STUB-CODE($resolver, $context);
            $code.apply-sink(False);
            $thunk.IMPL-QAST-BLOCK($context, :expression($code));
            $result := $thunk.meta-object()()
        }
        else {
            nqp::die('BEGIN time evaluation only supported for simple constructs so far')
        }

        self.IMPL-BOX-VM-VALUE($result)
    }

    # A native-typed expression evaluates to a bare VM-level box (for
    # example a BOOTInt) where the same expression at run time would
    # produce an Int. Such a box has no Raku method table and a null
    # WHO, so letting it escape into a stash breaks stash consumers
    # like the compunit GLOBAL merge. Box VM-level scalars into their
    # Raku types; leave VM-level aggregates alone, as constants holding
    # nqp hashes and lists rely on staying unboxed.
    # The result parameter must stay raw: a begin-time value may be a
    # container, like a Proxy a constant binds, and the entry decont the
    # method compiler emits for a non-raw parameter would strip it.
    method IMPL-BOX-VM-VALUE(Mu $result is raw) {
        unless nqp::isnull($result) {
            # A native reference must not escape either. The frame
            # holding the referenced slot is gone once this evaluation
            # returns, and a reference cannot be serialized. Snapshot
            # the referenced value, leaving a VM-level box for the
            # code below.
            if nqp::iscont_i($result) {
                $result := nqp::box_i(nqp::decont_i($result), nqp::bootint());
            }
            elsif nqp::iscont_u($result) {
                $result := nqp::box_u(nqp::decont_u($result), nqp::bootint());
            }
            elsif nqp::iscont_n($result) {
                $result := nqp::box_n(nqp::decont_n($result), nqp::bootnum());
            }
            elsif nqp::iscont_s($result) {
                $result := nqp::box_s(nqp::decont_s($result), nqp::bootstr());
            }
            my $type := nqp::what($result);
            $result := nqp::hllizefor($result, 'Raku')
              if nqp::eqaddr($type, nqp::bootint())
              || nqp::eqaddr($type, nqp::bootnum())
              || nqp::eqaddr($type, nqp::bootstr());
        }
        $result
    }

    # Evaluate the argument of a pragma, use, import, or require into an
    # argument list, boxing the result, which is consumed as a Raku List.
    method IMPL-BEGIN-TIME-ARGLIST(
                   RakuAST::Node $argument,
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        $argument
          ?? nqp::hllizefor(
               self.IMPL-BEGIN-TIME-EVALUATE($argument, $resolver, $context),
               'Raku'
             ).List.FLATTENABLE_LIST
          !! Nil
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
        my $*BEGIN-TIME-LOOKUP :=
          RakuAST::BeginTime.IMPL-BEGIN-TIME-LOOKUP-STATE($resolver, $context);

        # A primed argument (a WhateverCode) may be interpreted here, as
        # its static block compiles against a real QAST context.
        my $*IMPL-INTERPRET-PRIMED := 1;

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
