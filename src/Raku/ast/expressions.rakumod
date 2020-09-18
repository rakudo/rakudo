# Marker for anything that can be used as the source for a capture.
class RakuAST::CaptureSource is RakuAST::Node {
}

# Everything that can appear as an expression does RakuAST::Expression.
class RakuAST::Expression is RakuAST::Node {
}

# Everything that is termish (a term with prefixes or postfixes applied).
class RakuAST::Termish is RakuAST::Expression is RakuAST::CaptureSource {
}

# Everything that is a kind of term does RakuAST::Term.
class RakuAST::Term is RakuAST::Termish {
}

# Marker for all kinds of infixish operators.
class RakuAST::Infixish is RakuAST::Node {
    method IMPL-LIST-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operands) {
        nqp::die('Cannot compile ' ~ self.HOW.name(self) ~ ' as a list infix');
    }
}

# A simple (non-meta) infix operator. Some of these are just function calls, others
# need more special attention.
class RakuAST::Infix is RakuAST::Infixish is RakuAST::Lookup {
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Infix, '$!operator', $operator);
        $obj
    }

    method DEPARSE() { ' ' ~ $!operator ~ ' ' }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-infix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    # Returns True if this is a built-in short-circuit operator, and False if not.
    method short-circuit() {
        my constant SC := nqp::hash(
            '||', True,
            'or', True,
            '&&', True,
            'and', True,
            '//', True,
            'andthen', True,
            'notandthen', True,
            'orelse', True
        );
        SC{$!operator} // False
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        my str $op := $!operator;

        # Some ops map directly into a QAST primitive.
        my constant OP-TO-QAST-OP := nqp::hash(
            '||', 'unless',
            'or', 'unless',
            '&&', 'if',
            'and', 'if',
            '^^', 'xor',
            'xor', 'xor',
            '//', 'defor'
        );
        my $qast-op := OP-TO-QAST-OP{$op};
        if $qast-op {
            return QAST::Op.new( :op($qast-op), $left-qast, $right-qast );
        }

        # Otherwise, it's called by finding the lexical sub to call.
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('call'), :$name, $left-qast, $right-qast )
    }

    method IMPL-LIST-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operands) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name );
        for $operands {
            $op.push($_);
        }
        $op
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :sopce('lexical'), :$name )
    }

    method IMPL-CAN-INTERPRET() {
        !self.short-circuit && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx, List $operands) {
        my $op := self.resolution.compile-time-value;
        my @operands;
        for self.IMPL-UNWRAP-LIST($operands) {
            nqp::push(@operands, $_.IMPL-INTERPRET($ctx));
        }
        $op(|@operands)
    }
}

# A lookup of a chaining (non-meta) infix operator.
class RakuAST::Infix::Chaining is RakuAST::Infix is RakuAST::Lookup {
    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('chain'), :$name, $left-qast, $right-qast )
    }
}

# An assign meta-operator, operator on another infix.
class RakuAST::MetaInfix::Assign is RakuAST::Infixish is RakuAST::Lookup {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Assign, '$!infix', $infix);
        $obj
    }

    method DEPARSE() { ' ' ~ $!infix.operator ~ '= ' }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-infix('&METAOP_ASSIGN');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        # TODO case-analyzed assignments
        my $temp := QAST::Node.unique('meta_assign');
        my $bind-lhs := QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :decl('var'), :scope('local'), :name($temp) ),
            $left-qast
        );
        if nqp::istype($!infix, RakuAST::Infix) && $!infix.short-circuit {
            # Compile the short-circuit ones "inside out", so we can avoid the
            # assignment.
            QAST::Stmt.new(
                $bind-lhs,
                $!infix.IMPL-INFIX-QAST(
                    $context,
                    QAST::Var.new( :scope('local'), :name($temp) ),
                    QAST::Op.new(
                        :op('assign'),
                        QAST::Var.new( :scope('local'), :name($temp) ),
                        $right-qast
                    )
                )
            )
        }
        else {
            QAST::Stmt.new(
                $bind-lhs,
                QAST::Op.new(
                    :op('assign'),
                    QAST::Var.new( :scope('local'), :name($temp) ),
                    $!infix.IMPL-INFIX-QAST(
                        $context,
                        QAST::Var.new( :scope('local'), :name($temp) ),
                        $right-qast
                    )
                )
            )
        }
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('call'), :$name, $!infix.IMPL-HOP-INFIX-QAST($context) )
    }
}

# Application of an infix operator.
class RakuAST::ApplyInfix is RakuAST::Expression {
    has RakuAST::Infixish $.infix;
    has RakuAST::Expression $.left;
    has RakuAST::Expression $.right;

    method new(RakuAST::Infixish :$infix!, RakuAST::Expression :$left!,
            RakuAST::Expression :$right!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!left', $left);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!right', $right);
        $obj
    }

    method DEPARSE() {
        $!left.DEPARSE ~ ' ' ~ $!infix.DEPARSE ~ ' ' ~ $!right.DEPARSE
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!infix.IMPL-INFIX-QAST: $context,
            $!left.IMPL-TO-QAST($context),
            $!right.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!left);
        $visitor($!infix);
        $visitor($!right);
    }

    method IMPL-CAN-INTERPRET() { self.left.IMPL-CAN-INTERPRET && self.right.IMPL-CAN-INTERPRET && self.infix.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        return self.infix.IMPL-INTERPRET($ctx, nqp::list($!left, $!right));
    }
}

# Application of an list-precedence infix operator.
class RakuAST::ApplyListInfix is RakuAST::Expression {
    has RakuAST::Infixish $.infix;
    has List $.operands;

    method new(RakuAST::Infixish :$infix!, List :$operands!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyListInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyListInfix, '$!operands',
            nqp::islist($operands) ?? self.IMPL-WRAP-LIST($operands) !! $operands);
        $obj
    }

    method DEPARSE() {
        my $parts := nqp::list_s;
        for self.IMPL-UNWRAP-LIST($!operands) {
            nqp::push_s($parts,$_.DEPARSE)
        }
        nqp::elems($parts)
          ?? nqp::join($!infix.operator ~ ' ',$parts)
          !! '()'
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @operands;
        for self.IMPL-UNWRAP-LIST($!operands) {
            @operands.push($_.IMPL-TO-QAST($context));
        }
        $!infix.IMPL-LIST-INFIX-QAST: $context, @operands;
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
        for self.IMPL-UNWRAP-LIST($!operands) {
            $visitor($_);
        }
    }

    method IMPL-CAN-INTERPRET() {
        if $!infix.IMPL-CAN-INTERPRET {
            for self.IMPL-UNWRAP-LIST($!operands) {
                return False unless $_.IMPL-CAN-INTERPRET;
            }
            True
        }
        else {
            False
        }
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        $!infix.IMPL-INTERPRET($ctx, $!operands)
    }
}

# The base of all dotty infixes (`$foo .bar` or `$foo .= bar()`).
class RakuAST::DottyInfixish is RakuAST::Node {
    method new() { nqp::create(self) }
}

# The `.` dotty infix.
class RakuAST::DottyInfix::Call is RakuAST::DottyInfixish {
    method DEPARSE() { '.' }

    method IMPL-DOTTY-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $lhs-qast,
            RakuAST::Postfixish $rhs-ast) {
        $rhs-ast.IMPL-POSTFIX-QAST($context, $lhs-qast)
    }
}

# The `.=` dotty infix.
class RakuAST::DottyInfix::CallAssign is RakuAST::DottyInfixish {
    method DEPARSE() { '.=' }

    method IMPL-DOTTY-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $lhs-qast,
            RakuAST::Postfixish $rhs-ast) {
        # Store the target in a temporary, so we only evaluate it once.
        my $temp := QAST::Node.unique('meta_assign');
        my $bind-lhs := QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :decl('var'), :scope('local'), :name($temp) ),
            $lhs-qast
        );

        # Emit the assignment.
        # TODO case analyze these
        QAST::Stmt.new(
            $bind-lhs,
            QAST::Op.new(
                :op('assign'),
                QAST::Var.new( :scope('local'), :name($temp) ),
                $rhs-ast.IMPL-POSTFIX-QAST(
                    $context,
                    QAST::Var.new( :scope('local'), :name($temp) ),
                )
            )
        )
    }
}

# Application of an dotty infix operator. These are infixes that actually
# parse a postfix operation on their right hand side, and thus won't fit in
# the standard infix model.
class RakuAST::ApplyDottyInfix is RakuAST::Expression {
    has RakuAST::DottyInfixish $.infix;
    has RakuAST::Expression $.left;
    has RakuAST::Postfixish $.right;

    method new(RakuAST::DottyInfixish :$infix!, RakuAST::Expression :$left!,
            RakuAST::Postfixish :$right!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!left', $left);
        nqp::bindattr($obj, RakuAST::ApplyInfix, '$!right', $right);
        $obj
    }

    method DEPARSE() { $!left.DEPARSE ~ $!infix.DEPARSE ~ $!right.DEPARSE }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!infix.IMPL-DOTTY-INFIX-QAST: $context,
            $!left.IMPL-TO-QAST($context),
            $!right
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

    method DEPARSE() { $!operator }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-prefix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-PREFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('call'), :$name, $operand-qast )
    }
}

# Application of a prefix operator.
class RakuAST::ApplyPrefix is RakuAST::Termish {
    has RakuAST::Prefixish $.prefix;
    has RakuAST::Expression $.operand;

    method new(:$prefix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!prefix', $prefix);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!operand', $operand);
        $obj
    }

    method DEPARSE() { $!prefix.DEPARSE ~ $!operand.DEPARSE }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!prefix.IMPL-PREFIX-QAST($context, $!operand.IMPL-TO-QAST($context))
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

    method DEPARSE() { $!operator }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-postfix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('call'), :$name, $operand-qast )
    }
}

# A marker for all postcircumfixes. These each have relatively special
# compilation, so they get distinct nodes.
class RakuAST::Postcircumfix is RakuAST::Postfixish {
}

# A postcircumfix array index operator, possibly multi-dimensional.
class RakuAST::Postcircumfix::ArrayIndex is RakuAST::Postcircumfix is RakuAST::Lookup {
    has RakuAST::SemiList $.index;

    method new(RakuAST::SemiList $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::ArrayIndex, '$!index', $index);
        $obj
    }

    method DEPARSE() { '[' ~ $!index.DEPARSE ~ ']' }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical(
            nqp::elems(self.IMPL-UNWRAP-LIST($!index.statements)) > 1
                ?? '&postcircumfix:<[; ]>'
                !! '&postcircumfix:<[ ]>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        $op
    }
}

# A postcircumfix hash index operator, possibly multi-dimensional.
class RakuAST::Postcircumfix::HashIndex is RakuAST::Postcircumfix is RakuAST::Lookup {
    has RakuAST::SemiList $.index;

    method new(RakuAST::SemiList $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::HashIndex, '$!index', $index);
        $obj
    }

    method DEPARSE() { '{' ~ $!index.DEPARSE ~ '}' }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical(
            nqp::elems(self.IMPL-UNWRAP-LIST($!index.statements)) > 1
                ?? '&postcircumfix:<{; }>'
                !! '&postcircumfix:<{ }>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        $op
    }
}

# A postcircumfix literal hash index operator.
class RakuAST::Postcircumfix::LiteralHashIndex is RakuAST::Postcircumfix is RakuAST::Lookup {
    has RakuAST::QuotedString $.index;

    method new(RakuAST::QuotedString $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::LiteralHashIndex, '$!index', $index);
        $obj
    }

    method RESOLVE() { '<' ~ $!index.DEPARSE ~ '>' }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&postcircumfix:<{ }>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        $op
    }
}

# Application of an postfix operator.
class RakuAST::ApplyPostfix is RakuAST::Termish {
    has RakuAST::Postfixish $.postfix;
    has RakuAST::Expression $.operand;

    method new(:$postfix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!postfix', $postfix);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!operand', $operand);
        $obj
    }

    method DEPARSE() { $!operand.DEPARSE ~ $!postfix.DEPARSE }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!postfix.IMPL-POSTFIX-QAST($context, $!operand.IMPL-TO-QAST($context))
    }

    method visit-children(Code $visitor) {
        $visitor($!operand);
        $visitor($!postfix);
    }

    method IMPL-CAN-INTERPRET() { $!operand.IMPL-CAN-INTERPRET && $!postfix.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        $!postfix.IMPL-INTERPRET($ctx, -> { $!operand.IMPL-INTERPRET($ctx) })
    }
}

# The ternary conditional operator (?? !!).
class RakuAST::Ternary is RakuAST::Expression {
    has RakuAST::Expression $.condition;
    has RakuAST::Expression $.then;
    has RakuAST::Expression $.else;

    method new(RakuAST::Expression :$condition!, RakuAST::Expression :$then!,
            RakuAST::Expression :$else!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Ternary, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Ternary, '$!then', $then);
        nqp::bindattr($obj, RakuAST::Ternary, '$!else', $else);
        $obj
    }

    method DEPARSE() {
        $!condition.DEPARSE ~ ' ?? ' ~ $!then.DEPARSE ~ ' !! ' ~ $!else.DEPARSE
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('if'),
            $!condition.IMPL-TO-QAST($context),
            $!then.IMPL-TO-QAST($context),
            $!else.IMPL-TO-QAST($context),
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!then);
        $visitor($!else);
    }
}
