# Done by all things that want to perform some kind of effect at BEGIN time.
# They may do that effect before their children are visited in resolution or
# after; the default is after.
class RakuAST::BeginTime is RakuAST::Node {
    has int $!begin-performed;

    # Method implemented by a node to perform its begin-time side-effects.
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::die('Missing PERFORM-BEGIN implementation in ' ~ self.HOW.name(self))
    }

    # Should the BEGIN-time effects be performed before or after the parse of
    # this node?
    method is-begin-performed-before-children() { False }

    # Ensure the begin-time effects are performed.
    method ensure-begin-performed(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless $!begin-performed {
            self.PERFORM-BEGIN($resolver, $context);
            nqp::bindattr_i(self, RakuAST::BeginTime, '$!begin-performed', 1);
        }
        Nil
    }

    # Called when a BEGIN-time construct needs to evaluate code. Tries to
    # interpret simple things to avoid the cost of compilation.
    method IMPL-BEGIN-TIME-EVALUATE(RakuAST::Node $code, RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $code.IMPL-CHECK($resolver, $context, False);
        if $code.IMPL-CAN-INTERPRET {
            $code.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new)
        }
        else {
            nqp::die('BEGIN time evaluation only supported for simple constructs so far')
        }
    }

    # Called when a BEGIN-time construct wants to evaluate a resolved code
    # with a set of arguments.
    method IMPL-BEGIN-TIME-CALL(RakuAST::Node $callee, RakuAST::ArgList $args,
            RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $callee.is-resolved && nqp::istype($callee.resolution, RakuAST::CompileTimeValue) &&
                $args.IMPL-CAN-INTERPRET {
            my $resolved := $callee.resolution.compile-time-value;
            my @args := $args.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new);
            my @pos := @args[0];
            my %named := @args[1];
            return $resolved(|@pos, |%named);
        }
        else {
            nqp::die('BEGIN time calls only supported for simple constructs so far')
        }
    }
}
