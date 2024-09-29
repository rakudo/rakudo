# The base of all contextualizers.
class RakuAST::Contextualizer
  is RakuAST::Term
{
    # The thing to be contextualized.
    has RakuAST::Contextualizable $.target;

    method new(RakuAST::Contextualizable $target) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Contextualizer, '$!target', $target);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # TODO 6.c semantics with $/
        QAST::Op.new(
            :op('callmethod'), :name(self.IMPL-METHOD),
            $!target.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!target);
    }
}

# The item contextualizer.
class RakuAST::Contextualizer::Item
  is RakuAST::Contextualizer
{
    method IMPL-METHOD() { 'item' }
    method sigil { '$' }
}

# The list contextualizer.
class RakuAST::Contextualizer::List
  is RakuAST::Contextualizer
{
    method IMPL-METHOD() { 'cache' }
    method sigil { '@' }
}

# The hash contextualizer.
class RakuAST::Contextualizer::Hash
  is RakuAST::Contextualizer
{
    method IMPL-METHOD() { 'hash' }
    method sigil { '%' }
}
