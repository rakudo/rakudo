# An attaching AST node is one that wants to be somehow attached to a parent
# element, because it has a semantic relationship with it. For example, a
# a (has-scoped) method will want to attach to the immediately enclosing
# package, while a placeholder parameter like $^a will want to attach to the
# nearest scope that can carry a signature. When reached during resolution or
# check time, the `attach` method is invoked with the resolver, which can be
# used to discover target attachment nodes and interact with them.
class RakuAST::Attaching
  is RakuAST::Node
{
    method attach(RakuAST::Resolver $resolver) {
        nqp::die('attach not implemented for ' ~ self.HOW.name(self));
    }
}

# To be done by any AST node that is the target for an attaching node.
class RakuAST::AttachTarget
  is RakuAST::Node
{
    # Expected to return a (possibly empty) List of attach target names
    # for this node.  Must be supplied by the consuming class.
    method attach-target-names() {
        nqp::die('attach-target-names not implemented for ' ~ self.HOW.name(self));
    }

    # Expected to clear any existing attachments, so we don't attach
    # things more than once.  Must be supplied by the consuming class.
    method clear-attachments() {
        nqp::die('clear-attachments not implemented for ' ~ self.HOW.name(self));
    }
}
