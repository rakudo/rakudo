# Done by all things that want to perform some kind of effect at parse time
# (that is, immediately upon node creation). When the compiler is producing
# the node, it will ensure that parse time actions are performed either:
# * For leixcal scopes, immediately upon entry
# * For everything else, at the point they are produced by the action
# For a synthetic AST it is called top-down for nodes that are a lexical
# scope and bottom-up for everything else.
class RakuAST::ParseTime
  is RakuAST::Node
{
    has int $!parse-performed;

    # Method implemented by a node to perform its parse-time side-effects.
    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::die('Missing PERFORM-PARSE implementation in ' ~ self.HOW.name(self))
    }

    method ensure-parse-performed(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless $!parse-performed {
            self.PERFORM-PARSE($resolver, $context);
            nqp::bindattr_i(self, RakuAST::ParseTime, '$!parse-performed', 1);
        }
    }

    # Called when a BEGIN-time construct needs to evaluate code. Tries to
    # interpret simple things to avoid the cost of compilation.
    method IMPL-BEGIN-TIME-EVALUATE(RakuAST::Node $code, RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $code.IMPL-CAN-INTERPRET {
            $code.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new)
        }
        elsif nqp::istype($code, RakuAST::Code) {
            $code.meta-object;
        }
        elsif nqp::istype($code, RakuAST::Expression) {
            my $thunk := RakuAST::ExpressionThunk.new;
            $code.wrap-with-thunk($thunk);
            $thunk.IMPL-STUB-CODE($resolver, $context);
            $thunk.IMPL-QAST-BLOCK($context, :expression($code));
            $thunk.meta-object()()
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
