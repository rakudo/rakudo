# The base of all RakuAST nodes.
class RakuAST::Node {
    method type { Mu }

    # XXX temporary attributes/methods while we refactor to the new AST
    has Str $!named;
    method annotate($key, $value) {
    }
    method ann($key) {
        Mu
    }
    method wanted($value?) {
        1
    }
    method sunk($value?) {
        Mu
    }
    method returns($value?) {
        self.type
    }
    method has_compile_time_value() {
        0
    }
    method compile_time_value() {
        self.value
    }
    method flat() {
        Mu
    }
    method okifnil($vvalue?) {
        Mu
    }
    method named($value?) {
       if nqp::isconcrete($value) {
            nqp::bindattr(self, RakuAST::Node, '$!named', $value);
            $value
        }
        else {
            $!named || ''
        }
    }
    # XXX end temporaries

    # Entry point for production of a QAST compilation unit from the Raku AST
    method IMPL-TO-QAST-COMP-UNIT(Str :$comp-unit-name!, :$resolver!, *%options) {
        # Ensure fully resolved. TODO Maybe this should come earlier?
        self.resolve-all($resolver);

        # Create compilation context.
        my $sc := nqp::createsc($comp-unit-name);
        my $context := RakuAST::IMPL::QASTContext.new(:$sc);

        # Compile into a QAST::CompUnit.
        my $top-level := QAST::Block.new: self.IMPL-TO-QAST($context);
        QAST::CompUnit.new($top-level, :hll('Raku'), :$sc)
    }

    # Visits all child nodes of this one, applying the selected block.
    # This is a non-recursive operation.
    method visit-children($visitor) {
        # Default is that we have no children to visit.
        Nil
    }

    # Resolves all nodes beneath this one, recursively, using the specified
    # resolver.
    method resolve-all(RakuAST::Resolver $resolver) {
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved {
            self.resolve-with($resolver);
        }
        self.visit-children(-> $child { $child.resolve-all($resolver) });
        Nil
    }
}
