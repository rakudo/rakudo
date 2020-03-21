# Everything that can appear as an expression does RakuAST::Expression.
class RakuAST::Expression is RakuAST::Node {
}

# Everything that is a kind of term does RakuAST::Term.
class RakuAST::Term is RakuAST::Expression {
}

# Marker for all kinds of infixish operators.
class RakuAST::Infixish is RakuAST::Node {
}

# A lookup of a simple (non-meta) infix operator.
class RakuAST::Infix is RakuAST::Infixish is RakuAST::Lookup {
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Infix, '$!operator', $operator);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-infix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :$name, :scope('lexical') )
    }
}

# Application of an infix operator.
class RakuAST::ApplyInfix is RakuAST::Expression {
    has RakuAST::Infixish $.infix;
    has RakuAST::Expression $.left;
    has RakuAST::Expression $.right;

    method new(:$infix!, :$left!, :$right!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!left', $left);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!right', $right);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('call'),
            $!infix.IMPL-TO-QAST($context),
            $!left.IMPL-TO-QAST($context),
            $!right.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!left);
        $visitor($!infix);
        $visitor($!right);
    }
}

# Marker for all kinds of prefixish operators.
class RakuAST::Prefixish is RakuAST::Node {
}

# A lookup of a simple (non-meta) prefix operator.
class RakuAST::Prefix is RakuAST::Prefixish is RakuAST::Lookup {
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Prefix, '$!operator', $operator);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-prefix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :$name, :scope('lexical') )
    }
}

# Application of a prefix operator.
class RakuAST::ApplyPrefix is RakuAST::Expression {
    has RakuAST::Prefixish $.prefix;
    has RakuAST::Expression $.operand;

    method new(:$prefix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!prefix', $prefix);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!operand', $operand);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('call'),
            $!prefix.IMPL-TO-QAST($context),
            $!operand.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!prefix);
        $visitor($!operand);
    }
}

# Marker for all kinds of postfixish operators.
class RakuAST::Postfixish is RakuAST::Node {
}

# A lookup of a simple (non-meta) postfix operator.
class RakuAST::Postfix is RakuAST::Postfixish is RakuAST::Lookup {
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Postfix, '$!operator', $operator);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-postfix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :$name, :scope('lexical') )
    }
}

# Application of an postfix operator.
class RakuAST::ApplyPostfix is RakuAST::Expression {
    has RakuAST::Postfixish $.postfix;
    has RakuAST::Expression $.operand;

    method new(:$postfix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!postfix', $postfix);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!operand', $operand);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('call'),
            $!postfix.IMPL-TO-QAST($context),
            $!operand.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!operand);
        $visitor($!postfix);
    }
}
