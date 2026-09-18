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
            my $*BEGIN-TIME-LOOKUP :=
              RakuAST::Node.IMPL-BEGIN-TIME-LOOKUP-STATE($resolver, $context);
            self.PERFORM-BEGIN($resolver, $context);
            nqp::bindattr_i(self, RakuAST::BeginTime, '$!begin-performed', 1);
        }
        Nil
    }
}
