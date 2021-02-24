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
        my $resolved := $resolver.resolve-name($!name);
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

# A reduction meta-operator.
class RakuAST::Term::Reduce is RakuAST::Term is RakuAST::ImplicitLookups {
    has RakuAST::Infixish $.infix;
    has RakuAST::ArgList $.args;
    has Bool $.triangle;

    method new(RakuAST::Infixish :$infix!, RakuAST::ArgList :$args!, Bool :$triangle) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Reduce, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::Term::Reduce, '$!args', $args);
        nqp::bindattr($obj, RakuAST::Term::Reduce, '$!triangle', $triangle ?? True !! False);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&infix:<,>'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Make a call to form the meta-op.
        # TODO Cache it using a dispatcher when UNIT/SETTING operator
        my $reduce-helper := '&METAOP_REDUCE_LEFT'; # TODO Pick correct one
        my $form-meta := QAST::Op.new(
            :op<call>, :name($reduce-helper),
            $!infix.IMPL-HOP-INFIX-QAST($context));
        if $!triangle {
            $form-meta.push(QAST::WVal.new( :value(True) ));
        }

        # Invoke it with the arguments.
        if $!args.IMPL-IS-ONE-POS-ARG {
            # One-arg rule case, so just extract the argument
            my $arg := self.IMPL-UNWRAP-LIST($!args.args)[0];
            QAST::Op.new( :op<call>, $form-meta, $arg.IMPL-TO-QAST($context) )
        }
        else {
            # Form a List of the argumnets and pass it to the reducer
            # TODO thunk case
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $form-list := QAST::Op.new(
                :op('call'),
                :name(@lookups[0].resolution.lexical-name)
            );
            $!args.IMPL-ADD-QAST-ARGS($context, $form-list);
            QAST::Op.new( :op<call>, $form-meta, $form-list )
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
        $visitor($!args);
    }
}
