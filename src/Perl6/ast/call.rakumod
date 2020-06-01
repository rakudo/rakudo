# An argument list.
class RakuAST::ArgList is RakuAST::Node {
    has List $!args;

    method new(*@args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', @args);
        $obj
    }

    method args() {
        self.IMPL-WRAP-LIST($!args)
    }

    method visit-children(Code $visitor) {
        my @args := $!args;
        for @args {
            $visitor($_);
        }
    }

    method IMPL-ADD-QAST-ARGS(RakuAST::IMPL::QASTContext $context, QAST::Op $call) {
        for $!args {
            $call.push($_.IMPL-TO-QAST($context))
        }
    }
}

# Base role for all kinds of calls (named sub calls, calling some term, and
# method calls).
class RakuAST::Call is RakuAST::Node {
    has RakuAST::ArgList $.args;
}

# A call to a named sub.
class RakuAST::Call::Name is RakuAST::Term is RakuAST::Call is RakuAST::Lookup {
    has RakuAST::Name $.name;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::Name, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method needs-resolution() { $!name.is-identifier }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name($!name, :sigil('&'));
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $call := QAST::Op.new( :op('call') );
        if $!name.is-identifier {
            $call.name(self.resolution.lexical-name);
        }
        else {
            nqp::die('compiling complex call names NYI')
        }
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# A call to any term (the postfix () operator).
class RakuAST::Call::Term is RakuAST::Call is RakuAST::Postfixish {
    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $callee-qast) {
        my $call := QAST::Op.new( :op('call'), $callee-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}
