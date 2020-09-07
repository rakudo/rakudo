# A name, which may resolve to a type or a constant, may be indirect, etc.
# Things that are known to be types will instead be compiled into some
# kind of RakuAST::Type.
class RakuAST::Term::Name is RakuAST::Term is RakuAST::Lookup {
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Name, '$!name', $name);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        # TODO non-constants or indirects and so forth
        my $resolved := $resolver.resolve-name-constant($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # TODO indirects, trailing ::, etc.
        self.resolution.IMPL-LOOKUP-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# The self term for getting the current invocant
class RakuAST::Term::Self is RakuAST::Term is RakuAST::Lookup {
    method new() {
        nqp::create(self)
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('self');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# The term for a dotty operation on the current topic (for example in `.lc with $foo`).
class RakuAST::Term::TopicCall is RakuAST::Term is RakuAST::ImplicitLookups {
    has RakuAST::Postfixish $.call;

    method new(RakuAST::Postfixish $call) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::TopicCall, '$!call', $call);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $topic := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
        $!call.IMPL-POSTFIX-QAST($context, $topic.resolution.IMPL-LOOKUP-QAST($context))
    }

    method visit-children(Code $visitor) {
        $visitor($!call);
    }
}

# A named term that is implemented by a call to term:<foo>.
class RakuAST::Term::Named is RakuAST::Term is RakuAST::Lookup {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Term::Named, '$!name', $name);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-term($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The empty set term.
class RakuAST::Term::EmptySet is RakuAST::Term is RakuAST::Lookup {
    method new() {
        nqp::create(self)
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&set');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The rand term.
class RakuAST::Term::Rand is RakuAST::Term is RakuAST::Lookup {
    method new() {
        nqp::create(self)
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&rand');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The whatever (*) term.
class RakuAST::Term::Whatever is RakuAST::Term is RakuAST::Attaching {
    has RakuAST::CompUnit $!enclosing-comp-unit;

    method new() {
        nqp::create(self)
    }

    method attach(RakuAST::Resolver $resolver) {
        my $compunit := $resolver.find-attach-target('compunit');
        $compunit.ensure-singleton-whatever($resolver);
        nqp::bindattr(self, RakuAST::Term::Whatever, '$!enclosing-comp-unit', $compunit);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $whatever := $!enclosing-comp-unit.singleton-whatever();
        $context.ensure-sc($whatever);
        QAST::WVal.new( :value($whatever) )
    }
}

# The hyper whatever (**) term.
class RakuAST::Term::HyperWhatever is RakuAST::Term is RakuAST::Attaching {
    has RakuAST::CompUnit $!enclosing-comp-unit;

    method new() {
        nqp::create(self)
    }

    method attach(RakuAST::Resolver $resolver) {
        my $compunit := $resolver.find-attach-target('compunit');
        $compunit.ensure-singleton-hyper-whatever($resolver);
        nqp::bindattr(self, RakuAST::Term::HyperWhatever, '$!enclosing-comp-unit', $compunit);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $whatever := $!enclosing-comp-unit.singleton-hyper-whatever();
        $context.ensure-sc($whatever);
        QAST::WVal.new( :value($whatever) )
    }
}

# A capture term (`\$foo`, `\($foo, :bar)`).
class RakuAST::Term::Capture is RakuAST::Term {
    has RakuAST::CaptureSource $.source;

    method new(RakuAST::CaptureSource $source) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Capture, '$!source', $source);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $op := QAST::Op.new(
            :op('callmethod'),
            :name('from-args'),
            QAST::WVal.new( :value(Capture) ),
        );
        if nqp::istype($!source, RakuAST::ArgList) {
            $!source.IMPL-ADD-QAST-ARGS($context, $op);
        }
        else {
            $op.push($!source.IMPL-TO-QAST($context));
        }
        $op
    }

    method visit-children(Code $visitor) {
        $visitor($!source);
    }
}
