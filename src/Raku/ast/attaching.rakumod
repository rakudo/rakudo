# Done by any AST node that is the target for being attached to by another node.
# An attaching AST node is one that wants to be somehow attached to a parent
# element, because it has a semantic relationship with it. For example, a
# a (has-scoped) method will want to attach to the immediately enclosing
# package, while a placeholder parameter like $^a will want to attach to the
# nearest scope that can carry a signature. Attachment happens at parse time
# or BEGIN time.
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
