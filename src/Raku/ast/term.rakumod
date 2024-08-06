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

    method attach(RakuAST::Resolver $resolver) {
        my $package := $resolver.find-attach-target('package');
        nqp::bindattr(self, RakuAST::Term::Name, '$!package', $package);
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
        Nil
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        if self.name.is-pseudo-package {
            $resolver.build-exception: 'X::Bind'
        }
        else {
            $resolver.build-exception: 'X::Bind::Rebind',
                :target(self.name.canonicalize)
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        if $!name.is-pseudo-package {
            $!name.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP($context);
        }
        elsif $!name.is-package-lookup {
            return self.is-resolved
                ?? $!name.IMPL-QAST-PACKAGE-LOOKUP(
                    $context,
                    $!package,
                    :lexical(self.resolution)
                )
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

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# True
class RakuAST::Term::True {
    method new() {
        RakuAST::Term::Name.new(RakuAST::Name.from-identifier("True"))
    }
}

# False
class RakuAST::Term::False {
    method new() {
        RakuAST::Term::Name.new(RakuAST::Name.from-identifier("False"))
    }
}

# The self term for getting the current invocant
class RakuAST::Term::Self
  is RakuAST::Term
  is RakuAST::Lookup
  is RakuAST::ParseTime
  is RakuAST::CheckTime
{
    method new() {
        nqp::create(self)
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-lexical('self');
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless self.is-resolved {
            self.add-sorry($resolver.build-exception('X::Syntax::Self::WithoutObject'))
        }
    }

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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!call.IMPL-POSTFIX-QAST(
          $context,
          self.get-implicit-lookups.AT-POS(0).resolution.IMPL-LOOKUP-QAST($context)
        )
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
  is RakuAST::CheckTime
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
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&infix:<,>'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier($!infix.reducer-name)),
        ])
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
        my &reducer := self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
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
              self.get-implicit-lookups.AT-POS(0).resolution.lexical-name;
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
