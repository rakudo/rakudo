# Done by all things that want to perform some kind of effect at BEGIN time.
# They may do that effect before their children are visited in resolution or
# after; the default is after.
role RakuAST::BeginTime {
    has int $!begin-performed;

    # Method implemented by a node to perform its begin-time side-effects.
    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) { ... }

    # Ensure the begin-time effects are performed.
    method ensure-begin-performed(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        unless $!begin-performed {
            my $*BEGIN-TIME-LOOKUP :=
              RakuAST::Node.IMPL-BEGIN-TIME-LOOKUP-STATE($resolver, $context);
            self.PERFORM-BEGIN($resolver, $context);
            self.IMPL-MARK-BEGIN-PERFORMED;
        }
        Nil
    }

    # Record the begin-time effects as performed, which a PERFORM-BEGIN
    # whose work leads back to this node does first.
    method IMPL-MARK-BEGIN-PERFORMED() {
        nqp::bindattr_i(self, RakuAST::BeginTime, '$!begin-performed', 1);
    }
}
