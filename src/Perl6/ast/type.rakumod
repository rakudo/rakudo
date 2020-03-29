# Some kind of type (done by all kinds of things that result in a type).
class RakuAST::Type is RakuAST::Term {
}

# A simple type name, e.g. Int, Foo::Bar, etc.
class RakuAST::Type::Simple is RakuAST::Type is RakuAST::Lookup {
    # XXX Needs to handle qualified type names
    has str $.name;

    method new(Cool $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Type::Simple, '$!name', $name);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical-constant($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.resolution.compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}
