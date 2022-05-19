# Marker for anything that can be used as the source for a capture.
class RakuAST::CaptureSource is RakuAST::Node {
}

# Everything that can appear as an expression does RakuAST::Expression.
class RakuAST::Expression is RakuAST::Node {
    # All expressions can be thunked - that is, compiled such that they get
    # wrapped up in a code object of some kind. For such expressions, this
    # thunks attribute will point to a linked list of thunks to apply, the
    # outermost first. (Rationale: we'll add these at check time, and children
    # are visited ahead of parents. Adding to a linked list at the start is
    # cheapest.
    has Mu $!thunks;

    method wrap-with-thunk(RakuAST::ExpressionThunk $thunk) {
        $thunk.set-next($!thunks) if $!thunks;
        nqp::bindattr(self, RakuAST::Expression, '$!thunks', $thunk);
        Nil
    }

    method IMPL-QAST-ADD-THUNK-DECL-CODE(RakuAST::IMPL::QASTContext $context, Mu $target) {
        if $!thunks {
            $!thunks.IMPL-THUNK-CODE-QAST($context, $target, self);
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, *%opts) {
        $!thunks
            ?? $!thunks.IMPL-THUNK-VALUE-QAST($context)
            !! self.IMPL-EXPR-QAST($context, |%opts)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::die('Missing IMPL-EXPR-QAST method on ' ~ self.HOW.name(self))
    }

    method visit-thunks(Code $visitor) {
        my $cur-thunk := $!thunks;
        while $cur-thunk {
            $visitor($cur-thunk);
            $cur-thunk := $cur-thunk.next;
        }
    }
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

    # A node can implement this if it wishes to have full control of the
    # compilation of nodes. Most implement IMPL-INFIX-QAST, which gets the
    # QAST of the operands.
    method IMPL-INFIX-COMPILE(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $left, RakuAST::Expression $right) {
        self.IMPL-INFIX-QAST: $context, $left.IMPL-TO-QAST($context),
            $right.IMPL-TO-QAST($context)
    }
}

# A simple (non-meta) infix operator. Some of these are just function calls,
# others need more special attention.
class RakuAST::Infix is RakuAST::Infixish is RakuAST::Lookup {
    has str $.operator;
    has OperatorProperties $.properties;

    method new(str $operator, OperatorProperties :$properties) {
        $properties := OperatorProperties.properties-for-infix($operator)
            unless $properties;
        unless nqp::isconcrete($properties) {
            nqp::die("Failed to resolve operator properties for infix '$operator'");
        }
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Infix, '$!operator', $operator);
        nqp::bindattr($obj, RakuAST::Infix, '$!properties', $properties);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-infix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method reducer-name() { $!properties.reducer-name }

    method IMPL-INFIX-COMPILE(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $left, RakuAST::Expression $right) {
        my str $op := $!operator;
        if $op eq ':=' {
            if $left.can-be-bound-to {
                $left.IMPL-BIND-QAST($context, $right)
            }
            else {
                nqp::die('Cannot compile bind to ' ~ $left.HOW.name($left));
            }
        }
        else {
            self.IMPL-INFIX-QAST: $context, $left.IMPL-TO-QAST($context),
                $right.IMPL-TO-QAST($context)
        }
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

        # Others are compiler special forms.
        if $op eq '~~' {
            return self.IMPL-SMARTMATCH-QAST($context, $left-qast, $right-qast, 0);
        }
        elsif $op eq '!~~' {
            return self.IMPL-SMARTMATCH-QAST($context, $left-qast, $right-qast, 1);
        }

        # Otherwise, it's called by finding the lexical sub to call, and
        # compiling it as chaining if required.
        my $name := self.resolution.lexical-name;
        my str $call-op := $!properties.chaining ?? 'chain' !! 'call';
        QAST::Op.new( :op($call-op), :$name, $left-qast, $right-qast )
    }

    method IMPL-SMARTMATCH-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast,
            Mu $right-qast, int $negate) {
        my $accepts-call := QAST::Op.new(
            :op('callmethod'), :name('ACCEPTS'),
            $right-qast,
            QAST::Var.new( :name('$_'), :scope('lexical') )
        );
        if $negate {
            $accepts-call := QAST::Op.new(
                :op('hllbool'),
                QAST::Op.new(
                    :op('not_i'),
                    QAST::Op.new( :op('istrue'), $accepts-call )
                )
            );
        }
        self.IMPL-TEMPORARIZE-TOPIC($left-qast, $accepts-call)
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
        QAST::Var.new( :scope('lexical'), :$name )
    }

    method IMPL-CAN-INTERPRET() {
        !$!properties.short-circuit && !$!properties.chaining &&
            nqp::istype(self.resolution, RakuAST::CompileTimeValue)
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

# An assign meta-operator, operator on another infix.
class RakuAST::MetaInfix::Assign is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Assign, '$!infix', $infix);
        $obj
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
        if nqp::istype($!infix, RakuAST::Infix) && $!infix.properties.short-circuit {
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
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_ASSIGN'),
            $!infix.IMPL-HOP-INFIX-QAST($context)
    }
}

# A bracketed infix.
class RakuAST::BracketedInfix is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::BracketedInfix, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method reducer-name() { $!infix.reducer-name }

    method IMPL-INFIX-COMPILE(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $left, RakuAST::Expression $right) {
        $!infix.IMPL-INFIX-COMPILE($context, $left, $right)
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        $!infix.IMPL-INFIX-QAST($context, $left-qast, $right-qast)
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!infix.IMPL-HOP-INFIX-QAST($context)
    }
}

# A function infix (`$x [&func] $y`).
class RakuAST::FunctionInfix is RakuAST::Infixish {
    has RakuAST::Expression $.function;

    method new(RakuAST::Expression $function) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::FunctionInfix, '$!function', $function);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!function);
    }

    method reducer-name() { '&METAOP_REDUCE_LEFT' }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        QAST::Op.new:
            :op('call'),
            $!function.IMPL-TO-QAST($context),
            $left-qast, $right-qast
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!function.IMPL-TO-QAST($context)
    }
}

# A negate meta-operator.
class RakuAST::MetaInfix::Negate is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Negate, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        QAST::Op.new(
            :op('hllbool'),
            QAST::Op.new(
                :op('not_i'),
                QAST::Op.new(
                    :op('istrue'),
                    $!infix.IMPL-INFIX-QAST($context, $left-qast, $right-qast)
                )
            )
        )
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new:
            :op('call'), :name('&METAOP_NEGATE'),
            $!infix.IMPL-HOP-INFIX-QAST($context)
    }
}

# A reverse meta-operator.
class RakuAST::MetaInfix::Reverse is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Reverse, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        $!infix.IMPL-INFIX-QAST($context, $right-qast, $left-qast)
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_REVERSE'),
            $!infix.IMPL-HOP-INFIX-QAST($context)
    }
}

# A cross meta-operator.
class RakuAST::MetaInfix::Cross is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Cross, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-LIST-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operands) {
        my $op := QAST::Op.new( :op('call'), self.IMPL-HOP-INFIX-QAST($context) );
        for $operands {
            $op.push($_);
        }
        $op
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_CROSS'),
            $!infix.IMPL-HOP-INFIX-QAST($context),
            QAST::Var.new( :name($!infix.reducer-name), :scope('lexical') )
    }
}

# A zip meta-operator.
class RakuAST::MetaInfix::Zip is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Zip, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-LIST-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operands) {
        my $op := QAST::Op.new( :op('call'), self.IMPL-HOP-INFIX-QAST($context) );
        for $operands {
            $op.push($_);
        }
        $op
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_ZIP'),
            $!infix.IMPL-HOP-INFIX-QAST($context),
            QAST::Var.new( :name($!infix.reducer-name), :scope('lexical') )
    }
}

# An infix hyper operator.
class RakuAST::MetaInfix::Hyper is RakuAST::Infixish {
    has RakuAST::Infixish $.infix;
    has Bool $.dwim-left;
    has Bool $.dwim-right;

    method new(RakuAST::Infixish :$infix!, Bool :$dwim-left, Bool :$dwim-right) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Hyper, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::MetaInfix::Hyper, '$!dwim-left',
            $dwim-left ?? True !! False);
        nqp::bindattr($obj, RakuAST::MetaInfix::Hyper, '$!dwim-right',
            $dwim-right ?? True !! False);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method IMPL-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $left-qast, Mu $right-qast) {
        QAST::Op.new:
            :op('call'),
            self.IMPL-HOP-INFIX-QAST($context),
            $left-qast,
            $right-qast
    }

    method IMPL-HOP-INFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        my $call := QAST::Op.new:
            :op('callstatic'), :name('&METAOP_HYPER'),
            $!infix.IMPL-HOP-INFIX-QAST($context);
        if $!dwim-left {
            $call.push: QAST::WVal.new: :value(True), :named('dwim-left');
        }
        if $!dwim-right {
            $call.push: QAST::WVal.new: :value(True), :named('dwim-right');
        }
        $call
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!infix.IMPL-INFIX-COMPILE($context, $!left, $!right)
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method IMPL-DOTTY-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $lhs-qast,
            RakuAST::Postfixish $rhs-ast) {
        $rhs-ast.IMPL-POSTFIX-QAST($context, $lhs-qast)
    }
}

# The `.=` dotty infix.
class RakuAST::DottyInfix::CallAssign is RakuAST::DottyInfixish {

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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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

    method IMPL-HOP-PREFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :scope('lexical'), :$name )
    }
}

# The prefix hyper meta-operator.
class RakuAST::MetaPrefix::Hyper is RakuAST::Prefixish {
    has RakuAST::Prefix $.prefix;

    method new(RakuAST::Prefix $prefix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaPrefix::Hyper, '$!prefix', $prefix);
        $obj
    }

    method IMPL-PREFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        QAST::Op.new:
            :op('call'),
            QAST::Op.new(
                :op('callstatic'), :name('&METAOP_HYPER_PREFIX'),
                $!prefix.IMPL-HOP-PREFIX-QAST($context)
            ),
            $operand-qast
    }

    method visit-children(Code $visitor) {
        $visitor($!prefix);
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!prefix.IMPL-PREFIX-QAST($context, $!operand.IMPL-TO-QAST($context))
    }

    method visit-children(Code $visitor) {
        $visitor($!prefix);
        $visitor($!operand);
    }
}

# Marker for all kinds of postfixish operators.
class RakuAST::Postfixish is RakuAST::Node {
    method can-be-used-with-hyper() { False }
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

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new( :op('call'), :$name, $operand-qast )
    }

    method can-be-used-with-hyper() { True }

    method IMPL-POSTFIX-HYPER-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_HYPER_POSTFIX_ARGS'),
            $operand-qast,
            self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# The postfix exponentiation operator (2⁴⁵).
class RakuAST::Postfix::Power is RakuAST::Postfixish is RakuAST::Lookup {
    has Int $.power;

    method new(Int $power) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postfix::Power, '$!power', $power);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-postfix('ⁿ');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        $context.ensure-sc($!power);
        QAST::Op.new:
            :op('call'), :$name,
            $operand-qast,
            QAST::WVal.new( :value($!power) )
    }

    method can-be-used-with-hyper() { False }
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

    method can-be-bound-to() {
        True
    }

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

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, RakuAST::Expression $source) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        my $bind := $source.IMPL-TO-QAST($context);
        $bind.named('BIND');
        $op.push($bind);
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

    method can-be-bound-to() {
        True
    }

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

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, RakuAST::Expression $source) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        my $bind := $source.IMPL-TO-QAST($context);
        $bind.named('BIND');
        $op.push($bind);
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

    method can-be-bound-to() {
        True
    }

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

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, RakuAST::Expression $source) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        my $bind := $source.IMPL-TO-QAST($context);
        $bind.named('BIND');
        $op.push($bind);
        $op
    }
}

# An hyper operator on a postfix operator.
class RakuAST::MetaPostfix::Hyper is RakuAST::Postfixish is RakuAST::CheckTime {
    has RakuAST::Postfixish $.postfix;

    method new(RakuAST::Postfixish $postfix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaPostfix::Hyper, '$!postfix', $postfix);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver) {
        unless $!postfix.can-be-used-with-hyper {
            self.add-sorry: $resolver.build-exception: 'X::AdHoc',
                payload => 'Cannot hyper this postfix'
        }
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        $!postfix.IMPL-POSTFIX-HYPER-QAST($context, $operand-qast)
    }

    method visit-children(Code $visitor) {
        $visitor($!postfix);
    }
}

# Application of a postfix operator.
class RakuAST::ApplyPostfix is RakuAST::Termish {
    has RakuAST::Postfixish $.postfix;
    has RakuAST::Expression $.operand;

    method new(:$postfix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!postfix', $postfix);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!operand', $operand);
        $obj
    }

    method can-be-bound-to() {
        $!postfix.can-be-bound-to
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $postfix-ast := $!postfix.IMPL-POSTFIX-QAST($context, $!operand.IMPL-TO-QAST($context));
        # Method calls may be to a foreign language, and thus return
        # values may need type mapping into Raku land.
        nqp::istype($!postfix, RakuAST::Call::Methodish)
            ?? QAST::Op.new(:op<hllize>, $postfix-ast)
            !! $postfix-ast
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, RakuAST::Expression $source) {
        $!postfix.IMPL-BIND-POSTFIX-QAST($context, $!operand, $source)
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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
