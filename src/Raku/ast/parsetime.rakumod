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
}
