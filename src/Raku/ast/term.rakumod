# A name, which may resolve to a type or a constant, may be indirect, etc.
# Things that are known to be types will instead be compiled into some
# kind of RakuAST::Type.
class RakuAST::Term::Name
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    has RakuAST::Name $.name;
    has Mu $!package;

    method new(RakuAST::Name $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Name, '$!name', $name);
        $obj
    }

    method from-identifier(str $name) {
        self.new(RakuAST::Name.from-identifier($name))
    }

    method has-compile-time-value() {
        self.is-resolved && self.resolution.has-compile-time-value
    }

    method maybe-compile-time-value() {
        self.resolution.compile-time-value
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-name($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        elsif $!name.is-package-lookup {
            my $name := $!name.base-name;
            if $name.canonicalize {
                $resolved := $resolver.resolve-name($name);
                if $resolved {
                    my $v := $resolved.compile-time-value;
                    self.set-resolution($resolved);
                }
            }
        }
        elsif $!name.is-package-search {
            my $parts := $!name.IMPL-UNWRAP-LIST($!name.parts);
            nqp::shift($parts); # Remove leading ::
            my $name := RakuAST::Name.new(|$parts);
            $resolved := $resolver.resolve-name($name);
            if $resolved {
                self.set-resolution($resolved);
            }
        }
        Nil
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $name := $!name;
        if $name.is-pseudo-package
            ?? nqp::istype($name.first-part, RakuAST::Name::Part::Empty) && $name.base-name.is-empty && $name.has-colonpairs
            !! ! $name.is-package-lookup && ! $name.is-indirect-lookup && ! self.is-resolved
        {
            self.add-sorry:
                $resolver.build-exception: 'X::NoSuchSymbol', :symbol($!name.canonicalize);
        }
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        my $name   := self.name;
        my $target := $name.canonicalize;
        my $class  := 'X::Bind';
        $name.is-pseudo-package
          ?? ($target := "pseudo-package $target")
          !! ($class := 'X::Bind::Rebind');
        $resolver.build-exception: $class, :$target;
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        if $!name.is-pseudo-package {
            if self-is-resolved {
                self.resolution.IMPL-LOOKUP-QAST($context);
            }
            else {
                $!name.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP($context);
            }
        }
        elsif $!name.is-package-lookup {
            return self.is-resolved && !$!name.is-global-lookup
                ?? QAST::Op.new(:op<who>, self.resolution.IMPL-LOOKUP-QAST($context))
                !! $!name.IMPL-QAST-PACKAGE-LOOKUP(
                    $context,
                    $!package
                );
        }
        elsif $!name.is-indirect-lookup {
            return $!name.IMPL-QAST-INDIRECT-LOOKUP($context);
        }
        else {
            self.resolution.IMPL-LOOKUP-QAST($context)
        }
    }

    method IMPL-CAN-INTERPRET() {
        self.has-compile-time-value
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.maybe-compile-time-value
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# Core enums
class RakuAST::Term::Enum
  is RakuAST::Term::Name
{
    method IMPL-IS-CONSTANT() { True }
}

# True
class RakuAST::Term::True {
    method new() { RakuAST::Term::Enum.from-identifier("True") }
}

# False
class RakuAST::Term::False {
    method new() { RakuAST::Term::Enum.from-identifier("False") }
}

# The self term for getting the current invocant
class RakuAST::Term::Self
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    has RakuAST::Var::Attribute::Public $!variable;

    method new(RakuAST::Var::Attribute::Public :$variable) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Self, '$!variable', $variable // RakuAST::Var::Attribute::Public);
        $obj
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical('self');
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless self.is-resolved {
            my $resolved := $resolver.resolve-lexical('self');
            if $resolved {
                self.set-resolution($resolved);
            }
            else {
                self.add-sorry(
                    $!variable
                        ?? $resolver.build-exception('X::Syntax::NoSelf', :variable($!variable.name))
                        !! $resolver.build-exception('X::Syntax::Self::WithoutObject')
                )
            }
        }
    }

    method IMPL-IS-CONSTANT() { True }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

# The term for a dotty operation on the current topic (for example in `.lc with $foo`).
class RakuAST::Term::TopicCall
  is RakuAST::Term
  is RakuAST::ImplicitLookups
{
    has RakuAST::Postfixish $.call;

    method new(RakuAST::Postfixish $call) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::TopicCall, '$!call', $call);
        $obj
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!call.add-colonpair($pair);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('$_'),
        ]
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Avoid worries about sink context
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $postfix-ast := $!call.IMPL-POSTFIX-QAST(
          $context,
          self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.IMPL-LOOKUP-QAST($context)
        );
        nqp::istype($!call, RakuAST::Call::Methodish)
            ?? QAST::Op.new(:op<hllize>, $postfix-ast)
            !! $postfix-ast
    }

    method visit-children(Code $visitor) {
        $visitor($!call);
    }
}

# A named term that is implemented by a call to term:<foo>.
class RakuAST::Term::Named
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Term::Named, '$!name', $name);
        $obj
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-term($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The empty set term.
class RakuAST::Term::EmptySet
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    method new() {
        nqp::create(self)
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical('&set');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The rand term.
class RakuAST::Term::Rand
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
{
    method new() {
        nqp::create(self)
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical('&rand');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name(self.resolution.lexical-name) )
    }
}

# The whatever (*) term.
class RakuAST::Term::Whatever
  is RakuAST::Term
  is RakuAST::BeginTime
{
    has RakuAST::CompUnit $!enclosing-comp-unit;

    method new() {
        nqp::create(self)
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $compunit := $resolver.find-attach-target('compunit');
        $compunit.ensure-singleton-whatever($resolver);
        nqp::bindattr(self, RakuAST::Term::Whatever, '$!enclosing-comp-unit', $compunit);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $whatever := $!enclosing-comp-unit.singleton-whatever();
        $context.ensure-sc($whatever);
        QAST::WVal.new( :value($whatever), :returns($whatever) )
    }
}

# A specialized class to act as the actual lookup for the relevant RakuAST::ParameterTarget::Whatever
# This is what a Term::Whatever often -- but not always -- becomes.
class RakuAST::WhateverCode::Argument
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::BeginTime
{
    has RakuAST::Name $!name;

    method new() {
        my $obj := nqp::create(self);
        $obj
    }

    method set-name(RakuAST::Name $name) {
        nqp::bindattr(self, RakuAST::WhateverCode::Argument, '$!name', $name);
    }

    method name() {
        $!name.canonicalize
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::die('WhateverCode::Argument did not get a name') unless $!name;
        my $resolved := $resolver.resolve-name($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }

    method dump-markers() {
        '🔆'
    }
}

# The hyper whatever (**) term.
class RakuAST::Term::HyperWhatever
  is RakuAST::Term
  is RakuAST::BeginTime
{
    has RakuAST::CompUnit $!enclosing-comp-unit;

    method new() {
        nqp::create(self)
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $compunit := $resolver.find-attach-target('compunit');
        $compunit.ensure-singleton-hyper-whatever($resolver);
        nqp::bindattr(self, RakuAST::Term::HyperWhatever, '$!enclosing-comp-unit', $compunit);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $whatever := $!enclosing-comp-unit.singleton-hyper-whatever();
        $context.ensure-sc($whatever);
        QAST::WVal.new( :value($whatever) )
    }
}

# A capture term (`\$foo`, `\($foo, :bar)`).
class RakuAST::Term::Capture
  is RakuAST::Term
{
    has RakuAST::CaptureSource $.source;

    method new(RakuAST::CaptureSource $source) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Capture, '$!source', $source);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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
class RakuAST::Term::Reduce
  is RakuAST::Term
  is RakuAST::BeginTime
  is RakuAST::ImplicitLookups
{
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

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        (my $reason := self.properties.not-reducible)
          ?? self.add-sorry(
               $resolver.build-exception("X::Syntax::CannotMeta",
                 meta     => "reduce",
                 operator => self.infix.operator,
                 dba      => self.properties.dba,
                 reason   => $reason
               )
             )
          !! True
    }

    method properties() { $!infix.properties }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('&infix:<,>'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier($!infix.reducer-name)),
        ]
    }

    method PERFORM-BEGIN(Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $args := $!args.IMPL-UNWRAP-LIST($!args.args);
        if nqp::elems($args) == 1
            && nqp::istype((my $arg := $args[0]), RakuAST::Circumfix::Parentheses)
        {
            my $semilist := $arg.semilist;
            my $statements := $semilist.IMPL-UNWRAP-LIST($semilist.statements);
            if nqp::elems($statements) == 0 {
                return;
            }
            if $semilist.IMPL-IS-SINGLE-EXPRESSION {
                if nqp::istype($statements[0].expression, RakuAST::ApplyListInfix) && $statements[0].expression.IMPL-IS-LIST-LITERAL {
                    $args := self.IMPL-UNWRAP-LIST($statements[0].expression.operands);
                }
                else {
                    return; # No use in thunking a single argument. We'd have to evaluate it anyway.
                }
            }
        }
        $!infix.IMPL-THUNK-ARGUMENTS($resolver, $context, |$args, :meta);
    }

    method IMPL-HOP-INFIX() {
        my &reducer := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].resolution.compile-time-value;
        $!triangle ?? &reducer($!infix.IMPL-HOP-INFIX, True) !! &reducer($!infix.IMPL-HOP-INFIX)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # Make a call to form the meta-op.
        # TODO Cache it using a dispatcher when UNIT/SETTING operator
        my $form-meta := QAST::Op.new(
            :op<call>, :name($!infix.reducer-name),
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
            my $name :=
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.lexical-name;
            my $form-list := QAST::Op.new(:op('call'), :$name);
            $!args.IMPL-ADD-QAST-ARGS($context, $form-list);
            QAST::Op.new( :op<call>, $form-meta, $form-list )
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!infix);
        $visitor($!args);
    }
}

# A radix number, of the single-part form :10('foo') or the multi-part form
# :8[3,4,5].
class RakuAST::Term::RadixNumber
  is RakuAST::Term
{
    has Int $.radix;
    has RakuAST::Circumfix $.value;
    has Bool $.multi-part;

    method new(Int :$radix!, RakuAST::Circumfix :$value!, Bool :$multi-part) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::RadixNumber, '$!radix', $radix);
        nqp::bindattr($obj, RakuAST::Term::RadixNumber, '$!value', $value);
        nqp::bindattr($obj, RakuAST::Term::RadixNumber, '$!multi-part',
            $multi-part ?? True !! False);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $context.ensure-sc($!radix);
        QAST::Op.new:
            :op('call'), :name($!multi-part ?? '&UNBASE_BRACKET' !! '&UNBASE'),
            QAST::WVal.new( :value($!radix) ),
            $!value.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }
}

class RakuAST::Term::Declaration
  is RakuAST::Term
{
    has RakuAST::Declaration $.value;

    method new(RakuAST::Declaration $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::Declaration, '$!value', $value);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!value.IMPL-TO-QAST($context)
    }
}
