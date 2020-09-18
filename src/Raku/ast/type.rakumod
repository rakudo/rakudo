# Some kind of type (done by all kinds of things that result in a type).
class RakuAST::Type is RakuAST::Term {
}

# A simple type name, e.g. Int, Foo::Bar, etc.
class RakuAST::Type::Simple is RakuAST::Type is RakuAST::Lookup {
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Simple, '$!name', $name);
        $obj
    }

    method DEPARSE() { $!name.DEPARSE }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name-constant($!name);
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

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.resolution.compile-time-value
    }
}

# A simple type name, e.g. Int, Foo::Bar, etc. that should be looked up in the
# setting.
class RakuAST::Type::Setting is RakuAST::Type::Simple {
    # TODO limit lookup to setting
}
