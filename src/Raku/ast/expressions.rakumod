# Marker for anything that can be used as the source for a capture.
class RakuAST::CaptureSource
  is RakuAST::Node { }

# Everything that can appear as an expression does RakuAST::Expression.
class RakuAST::Expression
  is RakuAST::IMPL::ImmediateBlockUser
{
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

    method dump-extras(int $indent) {
        my $prefix := nqp::x(' ', $indent);
        my @chunks;
        self.visit-thunks(-> $thunk {
            @chunks.push("$prefix🧠 " ~ $thunk.thunk-kind ~ "\n");
            $thunk.visit-children(-> $child {
                @chunks.push($child.dump($indent + 2));
            });
        });
        nqp::join('', @chunks)
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

    method outer-most-thunk() {
        $!thunks
    }

    method IMPL-CURRY(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Str $arg) {
        my $thunk := RakuAST::CurryThunk.new($arg);
        $thunk.IMPL-CHECK($resolver, $context, True);
        self.wrap-with-thunk($thunk);
        $thunk
    }

    method IMPL-CURRIED() {
        my $cur-thunk := $!thunks;
        while $cur-thunk {
            return $cur-thunk if nqp::istype($cur-thunk, RakuAST::CurryThunk);
            $cur-thunk := $cur-thunk.next;
        }
        False
    }

    method IMPL-UNCURRY() {
        my $prev-thunk;
        my $cur-thunk := $!thunks;
        while $cur-thunk {
            if nqp::istype($cur-thunk, RakuAST::CurryThunk) {
                my $params := $cur-thunk.IMPL-PARAMS;
                if $prev-thunk {
                    $prev-thunk.set-next($cur-thunk.next);
                }
                else {
                    nqp::bindattr(self, RakuAST::Expression, '$!thunks', $cur-thunk.next);
                }
                return $params;
            }
            $prev-thunk := $cur-thunk;
            $cur-thunk := $cur-thunk.next;
        }
        nqp::die("UNCURRY didn't find a CurryThunk");
    }

    method IMPL-IMMEDIATELY-USES(RakuAST::Code $node) {
        $!thunks ?? True !! False
    }

    method IMPL-ADJUST-QAST-FOR-LVALUE(Mu $qast) {
        $qast
    }
}

# Everything that is termish (a term with prefixes or postfixes applied).
class RakuAST::Termish
  is RakuAST::Expression
  is RakuAST::CaptureSource { }

# Everything that is a kind of term does RakuAST::Term.
class RakuAST::Term
  is RakuAST::Termish { }

# Marker for all kinds of infixish operators.
class RakuAST::Infixish
  is RakuAST::ImplicitLookups
{
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

    method IMPL-THUNK-ARGUMENTS(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context,
                                RakuAST::Expression *@operands) {
    }

    # %curried == 0 means do not curry
    # %curried == 1 means curry Whatever only
    # %curried == 2 means curry WhateverCode only
    # %curried == 3 means curry both Whatever and WhateverCode (default)
    method IMPL-CURRIES() { 0 }
}

# A simple (non-meta) infix operator. Some of these are just function calls,
# others need more special attention.
class RakuAST::Infix
  is RakuAST::Infixish
  is RakuAST::Lookup
{
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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Match')),
        ])
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-infix($!operator);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method reducer-name() { $!properties.reducer-name }

    method IMPL-THUNK-ARGUMENT(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context,
                               RakuAST::Expression $expression, str $type) {
        if $type eq 'b' && !nqp::istype($expression, RakuAST::Code) {
            my $thunk := RakuAST::BlockThunk.new;
            $thunk.IMPL-CHECK($resolver, $context, True);
            $expression.wrap-with-thunk($thunk);
        }
        elsif $type eq 't' && !nqp::istype($expression, RakuAST::Code) {
            my $thunk := RakuAST::ExpressionThunk.new;
            $thunk.IMPL-CHECK($resolver, $context, True);
            $expression.wrap-with-thunk($thunk);
        }
        # TODO implement other thunk types
    }

    method IMPL-THUNK-ARGUMENTS(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context,
                                RakuAST::Expression *@operands) {
        if (
               $!operator eq 'xx'     || $!operator eq 'andthen'
            || $!operator eq 'orelse' || $!operator eq 'notandthen'
            || $!operator eq 'with'   || $!operator eq 'without'
        ) {
            my $thunky := $!properties.thunky;
            my $i := 0;
            for @operands {
                my $type := nqp::substr($thunky, $i, $i + 1);
                if $type && $type ne '.' {
                    self.IMPL-THUNK-ARGUMENT($resolver, $context, $_, $type);
                }
                $i++ if $i < nqp::chars($thunky) - 1;
            }
        }
    }

    method IMPL-CURRIES() {
        my constant CURRIED := nqp::hash(
            '...'   , 0,
            '…'     , 0,
            '...^'  , 0,
            '…^'    , 0,
            '^...'  , 0,
            '^…'    , 0,
            '^...^' , 0,
            '^…^'   , 0,
            '='     , 0,
            ':='    , 0,
            '&&',   , 0,
            '||',   , 0,
            '~~'    , 1,
            '∘'     , 1,
            'o'     , 1,
            '..'    , 2,
            '..^'   , 2,
            '^..'   , 2,
            '^..^'  , 2,
            'xx'    , 2,
        );
        CURRIED{$!operator} // 3
    }

    method IMPL-INFIX-COMPILE(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $left, RakuAST::Expression $right) {
        # Hash value is negation flag
        my constant OP-SMARTMATCH := nqp::hash( '~~', 0, '!~~', 1 );
        my str $op := $!operator;
        if $op eq ':=' {
            if $left.can-be-bound-to {
                $left.IMPL-BIND-QAST($context, $right.IMPL-TO-QAST($context))
            }
            else {
                nqp::die('Cannot compile bind to ' ~ $left.HOW.name($left));
            }
        }
        elsif nqp::existskey(OP-SMARTMATCH, $op) && !nqp::istype($right, RakuAST::Var) {
            self.IMPL-SMARTMATCH-QAST($context, $left, $right, nqp::atkey(OP-SMARTMATCH, $op));
        }
        else {
            self.IMPL-INFIX-QAST:
                $context,
                $op eq '='
                    ?? $left.IMPL-ADJUST-QAST-FOR-LVALUE($left.IMPL-TO-QAST($context))
                    !! $left.IMPL-TO-QAST($context),
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

        # Otherwise, it's called by finding the lexical sub to call, and
        # compiling it as chaining if required.
        my $name := self.resolution.lexical-name;
        my str $call-op := $!properties.chaining ?? 'chain' !! 'call';
        QAST::Op.new( :op($call-op), :$name, $left-qast, $right-qast )
    }

    method IMPL-SMARTMATCH-QAST( RakuAST::IMPL::QASTContext $context,
                                 RakuAST::Expression $left,
                                 RakuAST::Expression $right,
                                 int $negate ) {
        # Handle cases of s/// or m// separately. For a non-negating smartmatch this case could've been reduced to
        # plain topic localization except that we must ensure a False returned when there is no match.
        if nqp::istype($right, RakuAST::RegexThunk)
            && (!nqp::can($right, 'match-immediately') || $right.match-immediately)
        {
            my $match-type :=
              self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
            my $result-local := QAST::Node.unique('!sm-result');
            my $rhs := $right.IMPL-EXPR-QAST($context);
            return self.IMPL-TEMPORARIZE-TOPIC(
                $left.IMPL-TO-QAST($context),
                $negate
                    ?? QAST::Op.new( :op<callmethod>, :name<not>, $rhs)
                    !! QAST::Op.new( :op<unless>, $rhs, QAST::WVal.new( :value(False) )));
        }

        my $accepts-call;
        if $negate {
            $accepts-call := QAST::Op.new(
                :op<callmethod>, :name<not>,
                QAST::Op.new(
                    :op('callmethod'), :name('ACCEPTS'),
                    $right.IMPL-TO-QAST($context),
                    QAST::Var.new(:name<$_>, :scope<lexical>)));
        }
        else {
            my $rhs-local := QAST::Node.unique('!sm-rhs');
            $accepts-call := QAST::Op.new(
                :op('callmethod'), :name('ACCEPTS'),
                QAST::Var.new( :name($rhs-local), :scope<local> ),
                QAST::Var.new(:name<$_>, :scope<lexical>));
            $accepts-call := QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<istype>,
                    QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new( :name($rhs-local), :scope<local>, :decl<var> ),
                        $right.IMPL-TO-QAST($context),
                    ),
                    QAST::WVal.new( :value(Regex) )),
                $accepts-call,
                QAST::Op.new(
                    :op<callmethod>,
                    :name<Bool>,
                    $accepts-call ));
        }
        self.IMPL-TEMPORARIZE-TOPIC( $left.IMPL-TO-QAST($context), $accepts-call )
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

# Meta infixes base class, mostly for type checking
class RakuAST::MetaInfix
  is RakuAST::Infixish {
    method IMPL-HOP-INFIX() {
        self.get-implicit-lookups().AT-POS(0).resolution.compile-time-value()(
            self.infix.resolution.compile-time-value
        )
    }
}

# An assign meta-operator, operator on another infix.
class RakuAST::MetaInfix::Assign
  is RakuAST::MetaInfix
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Assign, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_ASSIGN')),
        ])
    }

    method IMPL-CURRIES() { 0 }

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
class RakuAST::BracketedInfix
  is RakuAST::Infixish
{
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
class RakuAST::FunctionInfix
  is RakuAST::Infixish
{
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
class RakuAST::MetaInfix::Negate
  is RakuAST::MetaInfix
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Negate, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_NEGATE')),
        ])
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
class RakuAST::MetaInfix::Reverse
  is RakuAST::MetaInfix
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Reverse, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_REVERSE')),
        ])
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
class RakuAST::MetaInfix::Cross
  is RakuAST::MetaInfix
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Cross, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_CROSS')),
        ])
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
class RakuAST::MetaInfix::Zip
  is RakuAST::MetaInfix
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish $infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaInfix::Zip, '$!infix', $infix);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_ZIP')),
        ])
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
class RakuAST::MetaInfix::Hyper
  is RakuAST::MetaInfix
{
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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&METAOP_HYPER')),
        ])
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

    method IMPL-HOP-INFIX() {
        self.get-implicit-lookups().AT-POS(0).resolution.compile-time-value()(
            self.infix.resolution.compile-time-value,
            :dwim-left($!dwim-left),
            :dwim-right($!dwim-right)
        )
    }
}

# Application of an infix operator.
class RakuAST::ApplyInfix
  is RakuAST::Expression
  is RakuAST::BeginTime
{
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

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::bitand_i($!infix.IMPL-CURRIES, 2) && (my $curried := $!left.IMPL-CURRIED) {
            my $params := $!left.IMPL-UNCURRY;
            self.IMPL-CURRY($resolver, $context, '') unless self.IMPL-CURRIED;
            $curried := self.IMPL-CURRIED;
            for $params {
                $curried.IMPL-ADD-PARAM($_.target.lexical-name);
            }
        }
        if nqp::bitand_i($!infix.IMPL-CURRIES, 1) {
            for '$!left', '$!right' {
                my $operand := nqp::getattr(self, RakuAST::ApplyInfix, $_);
                if nqp::istype($operand, RakuAST::Term::Whatever) {
                    my $curried := self.IMPL-CURRIED;
                    my $param_name := '$whatevercode_arg_' ~ ($curried ?? $curried.IMPL-NUM-PARAMS + 1 !! 1);
                    my $param;
                    if $curried {
                        $param := $curried.IMPL-ADD-PARAM($param_name);
                        $curried.IMPL-CHECK($resolver, $context, True);
                    }
                    else {
                        $param := self.IMPL-CURRY($resolver, $context, $param_name).IMPL-LAST-PARAM;
                    }
                    nqp::bindattr(self, RakuAST::ApplyInfix, $_, $param.target.generate-lookup);
                }
            }
        }
        if nqp::bitand_i($!infix.IMPL-CURRIES, 2) && ($curried := $!right.IMPL-CURRIED) {
            my $params := $!right.IMPL-UNCURRY;
            self.IMPL-CURRY($resolver, $context, '') unless self.IMPL-CURRIED;
            $curried := self.IMPL-CURRIED;
            my $param-num := $curried.IMPL-NUM-PARAMS;
            for $params {
                $param-num++;
                $_.target.set-name('$whatevercode_arg_' ~ $param-num);
                $curried.IMPL-ADD-PARAM($_.target.lexical-name);
            }
        }

        $!infix.IMPL-THUNK-ARGUMENTS($resolver, $context, $!left, $!right);
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
class RakuAST::ApplyListInfix
  is RakuAST::Expression
  is RakuAST::BeginTime
{
    has RakuAST::Infixish $.infix;
    has List $!operands;

    method new(RakuAST::Infixish :$infix!, List :$operands!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyListInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyListInfix, '$!operands', my $list := []);
        for self.IMPL-UNWRAP-LIST($operands) {
            if nqp::istype($_, RakuAST::ColonPairs) {
                for $_.colonpairs {
                    nqp::push($list, $_);
                }
            }
            else {
                nqp::push($list, $_);
            }
        }
        $obj
    }

    method operands() {
        self.IMPL-WRAP-LIST($!operands)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @operands;
        for $!operands {
            @operands.push($_.IMPL-TO-QAST($context));
        }
        $!infix.IMPL-LIST-INFIX-QAST: $context, @operands;
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
        for $!operands {
            $visitor($_);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $!infix.IMPL-THUNK-ARGUMENTS($resolver, $context, |self.IMPL-UNWRAP-LIST($!operands));
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
class RakuAST::DottyInfixish
  is RakuAST::Node
{
    method new() { nqp::create(self) }
}

# The `.` dotty infix.
class RakuAST::DottyInfix::Call
  is RakuAST::DottyInfixish
{

    method IMPL-DOTTY-INFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $lhs-qast,
            RakuAST::Postfixish $rhs-ast) {
        $rhs-ast.IMPL-POSTFIX-QAST($context, $lhs-qast)
    }
}

# The `.=` dotty infix.
class RakuAST::DottyInfix::CallAssign
  is RakuAST::DottyInfixish
{

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
class RakuAST::ApplyDottyInfix
  is RakuAST::Expression
{
    has RakuAST::DottyInfixish $.infix;
    has RakuAST::Expression $.left;
    has RakuAST::Postfixish $.right;

    method new(RakuAST::DottyInfixish :$infix!, RakuAST::Expression :$left!,
            RakuAST::Postfixish :$right!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyDottyInfix, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::ApplyDottyInfix, '$!left', $left);
        nqp::bindattr($obj, RakuAST::ApplyDottyInfix, '$!right', $right);
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
class RakuAST::Prefixish
  is RakuAST::Node
{
    has List $.colonpairs;

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!colonpairs.push: $pair;
    }

    method visit-colonpairs(Code $visitor) {
        for $!colonpairs {
            $visitor($_);
        }
    }

    method IMPL-ADD-COLONPAIRS-TO-OP(RakuAST::IMPL::QASTContext $context, Mu $op) {
        for $!colonpairs {
            my $val-ast := $_.named-arg-value.IMPL-TO-QAST($context);
            $val-ast.named($_.named-arg-name);
            $op.push($val-ast);
        }
    }

    method IMPL-CURRIES() { 3 }
}

# A lookup of a simple (non-meta) prefix operator.
class RakuAST::Prefix
  is RakuAST::Prefixish
  is RakuAST::Lookup
{
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Prefix, '$!operator', $operator);
        nqp::bindattr($obj, RakuAST::Prefixish, '$!colonpairs', []);
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
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        $op
    }

    method IMPL-HOP-PREFIX-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :scope('lexical'), :$name )
    }
}

# The prefix hyper meta-operator.
class RakuAST::MetaPrefix::Hyper
  is RakuAST::Prefixish
{
    has RakuAST::Prefix $.prefix;

    method new(RakuAST::Prefix $prefix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaPrefix::Hyper, '$!prefix', $prefix);
        nqp::bindattr($obj, RakuAST::Prefixish, '$!colonpairs', []);
        $obj
    }

    method IMPL-PREFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $op := QAST::Op.new:
            :op('call'),
            QAST::Op.new(
                :op('callstatic'), :name('&METAOP_HYPER_PREFIX'),
                $!prefix.IMPL-HOP-PREFIX-QAST($context)
            ),
            $operand-qast;
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        $op
    }

    method visit-children(Code $visitor) {
        $visitor($!prefix);
    }
}

# Application of a prefix operator.
class RakuAST::ApplyPrefix
  is RakuAST::Termish
  is RakuAST::BeginTime
{
    has RakuAST::Prefixish $.prefix;
    has RakuAST::Expression $.operand;

    method new(:$prefix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!prefix', $prefix);
        nqp::bindattr($obj, RakuAST::ApplyPrefix, '$!operand', $operand);
        $obj
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!prefix.add-colonpair($pair);
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::bitand_i($!prefix.IMPL-CURRIES, 1) {
            if nqp::istype($!operand, RakuAST::Term::Whatever) {
                nqp::bindattr(self, RakuAST::ApplyPrefix, '$!operand', RakuAST::Var::Lexical.new('$_'));
                self.IMPL-CURRY($resolver, $context, '$_');
                $!operand.resolve-with($resolver);
            }
        }
        if nqp::bitand_i($!prefix.IMPL-CURRIES, 2) {
            if $!operand.IMPL-CURRIED {
                $!operand.IMPL-UNCURRY;
                self.IMPL-CURRY($resolver, $context, '$_');
            }
        }
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
class RakuAST::Postfixish
  is RakuAST::Node
{
    has List $.colonpairs;

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!colonpairs.push: $pair;
    }

    method visit-colonpairs(Code $visitor) {
        for $!colonpairs {
            $visitor($_);
        }
    }

    method IMPL-ADD-COLONPAIRS-TO-OP(RakuAST::IMPL::QASTContext $context, Mu $op) {
        for $!colonpairs {
            my $val-ast := $_.named-arg-value.IMPL-TO-QAST($context);
            $val-ast.named($_.named-arg-name);
            $op.push($val-ast);
        }
    }

    # %curried == 0 means do not curry
    # %curried == 1 means curry Whatever only
    # %curried == 2 means curry WhateverCode only
    # %curried == 3 means curry both Whatever and WhateverCode (default)
    method IMPL-CURRIES() { 0 }

    method can-be-used-with-hyper() { False }
}

# A lookup of a simple (non-meta) postfix operator.
class RakuAST::Postfix
  is RakuAST::Postfixish
  is RakuAST::Lookup
{
    has str $.operator;

    method new(str $operator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Postfix, '$!operator', $operator);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
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

    method IMPL-CURRIES() { 3 }
}

# The postfix exponentiation operator (2⁴⁵).
class RakuAST::Postfix::Power
  is RakuAST::Postfixish
  is RakuAST::Lookup
{
    has Int $.power;

    method new(Int $power) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postfix::Power, '$!power', $power);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
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

    method IMPL-CURRIES() { 3 }
}

# A marker for all postcircumfixes. These each have relatively special
# compilation, so they get distinct nodes.
class RakuAST::Postcircumfix
  is RakuAST::Postfixish { }

# A postcircumfix array index operator, possibly multi-dimensional.
class RakuAST::Postcircumfix::ArrayIndex
  is RakuAST::Postcircumfix
  is RakuAST::Lookup
{
    has RakuAST::SemiList $.index;
    has RakuAST::Expression $.assignee;

    method new(RakuAST::SemiList :$index!, RakuAST::Expression :$assignee) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::ArrayIndex, '$!index', $index);
        nqp::bindattr($obj, RakuAST::Postcircumfix::ArrayIndex, '$!assignee', $assignee // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
        $obj
    }

    method set-assignee(RakuAST::Expression $assignee) {
        nqp::bindattr(self, RakuAST::Postcircumfix::ArrayIndex, '$!assignee', $assignee);
    }

    method can-be-bound-to() {
        True
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical(
            nqp::elems($!index.code-statements) > 1
                ?? '&postcircumfix:<[; ]>'
                !! '&postcircumfix:<[ ]>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
        $visitor($!assignee) if $!assignee;
        self.visit-colonpairs($visitor);
    }

    method IMPL-CURRIES() { 3 }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        $op.push($!assignee.IMPL-TO-QAST($context)) if $!assignee;
        $op
    }

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, QAST::Node $source-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        my $bind := $source-qast;
        $bind.named('BIND');
        $op.push($bind);
        $op
    }

    method can-be-used-with-hyper() { True }

    method IMPL-POSTFIX-HYPER-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        QAST::Op.new:
            :op('callstatic'), :name('&METAOP_HYPER_POSTFIX_ARGS'),
            $operand-qast,
            $!index.IMPL-TO-QAST($context),
            self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# A postcircumfix hash index operator, possibly multi-dimensional.
class RakuAST::Postcircumfix::HashIndex
  is RakuAST::Postcircumfix
  is RakuAST::Lookup
{
    has RakuAST::SemiList $.index;

    method new(RakuAST::SemiList $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::HashIndex, '$!index', $index);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
        $obj
    }

    method can-be-bound-to() {
        True
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical(
            nqp::elems($!index.code-statements) > 1
                ?? '&postcircumfix:<{; }>'
                !! '&postcircumfix:<{ }>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!index);
        self.visit-colonpairs($visitor);
    }

    method IMPL-CURRIES() { 3 }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        $op
    }

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, QAST::Node $source-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty;
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        my $bind := $source-qast;
        $bind.named('BIND');
        $op.push($bind);
        $op
    }
}

# A postcircumfix literal hash index operator.
class RakuAST::Postcircumfix::LiteralHashIndex
  is RakuAST::Postcircumfix
  is RakuAST::Lookup
{
    has RakuAST::QuotedString $.index;
    has RakuAST::Expression $.assignee;

    method new(RakuAST::QuotedString :$index!, RakuAST::Expression :$assignee) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Postcircumfix::LiteralHashIndex, '$!index', $index);
        nqp::bindattr($obj, RakuAST::Postcircumfix::LiteralHashIndex, '$!assignee', $assignee // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
        $obj
    }

    method set-assignee(RakuAST::Expression $assignee) {
        nqp::bindattr(self, RakuAST::Postcircumfix::LiteralHashIndex, '$!assignee', $assignee);
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
        $visitor($!assignee) if $!assignee;
        self.visit-colonpairs($visitor);
    }

    method IMPL-CURRIES() { 3 }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand-qast );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        $op.push($!assignee.IMPL-TO-QAST($context)) if $!assignee;
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        $op
    }

    method IMPL-BIND-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context,
            RakuAST::Expression $operand, QAST::Node $source-qast) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name, $operand.IMPL-TO-QAST($context) );
        $op.push($!index.IMPL-TO-QAST($context)) unless $!index.is-empty-words;
        self.IMPL-ADD-COLONPAIRS-TO-OP($context, $op);
        my $bind := $source-qast;
        $bind.named('BIND');
        $op.push($bind);
        $op
    }
}

# An hyper operator on a postfix operator.
class RakuAST::MetaPostfix::Hyper
  is RakuAST::Postfixish
  is RakuAST::CheckTime
{
    has RakuAST::Postfixish $.postfix;

    method new(RakuAST::Postfixish $postfix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::MetaPostfix::Hyper, '$!postfix', $postfix);
        nqp::bindattr($obj, RakuAST::Postfixish, '$!colonpairs', []);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
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
        self.visit-colonpairs($visitor);
    }
}

# Application of a postfix operator.
class RakuAST::ApplyPostfix
  is RakuAST::Termish
  is RakuAST::BeginTime
{
    has RakuAST::Postfixish $.postfix;
    has RakuAST::Expression $.operand;

    method new(:$postfix!, :$operand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!postfix', $postfix);
        if nqp::istype($operand, RakuAST::Circumfix::Parentheses)
            && $operand.semilist.IMPL-IS-SINGLE-EXPRESSION
        {
            my $statement :=
              self.IMPL-UNWRAP-LIST($operand.semilist.code-statements)[0];
            $operand := $statement.expression
                unless $statement.condition-modifier || $statement.loop-modifier;
        }
        nqp::bindattr($obj, RakuAST::ApplyPostfix, '$!operand', $operand);
        $obj
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!postfix.add-colonpair($pair);
    }

    method can-be-bound-to() {
        $!postfix.can-be-bound-to
    }

    method on-topic() {
        nqp::istype($!operand,RakuAST::Var::Lexical) && $!operand.name eq '$_'
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::bitand_i($!postfix.IMPL-CURRIES, 1) {
            if nqp::istype($!operand, RakuAST::Term::Whatever) {
                nqp::bindattr(self, RakuAST::ApplyPostfix, '$!operand', RakuAST::Var::Lexical.new('$_'));
                self.IMPL-CURRY($resolver, $context, '$_');
                $!operand.resolve-with($resolver);
            }
        }
        if nqp::bitand_i($!postfix.IMPL-CURRIES, 2) {
            if $!operand.IMPL-CURRIED {
                $!operand.IMPL-UNCURRY;
                self.IMPL-CURRY($resolver, $context, '$_');
            }
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $postfix-ast := $!postfix.IMPL-POSTFIX-QAST($context, $!operand.IMPL-TO-QAST($context));
        # Method calls may be to a foreign language, and thus return
        # values may need type mapping into Raku land.
        nqp::istype($!postfix, RakuAST::Call::Methodish)
            ?? QAST::Op.new(:op<hllize>, $postfix-ast)
            !! $postfix-ast
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        $!postfix.IMPL-BIND-POSTFIX-QAST($context, $!operand, $source-qast)
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
class RakuAST::Ternary
  is RakuAST::Expression
{
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
