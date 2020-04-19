# All initializers do this marker. An initializer is the `= 42` part in a
# declaration like `my $a = 42`.
class RakuAST::Initializer is RakuAST::Node {
    method is-binding() { False }
}

# An assignment (`=`) initializer.
class RakuAST::Initializer::Assign is RakuAST::Initializer {
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Initializer::Assign, '$!expression', $expression);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!expression.IMPL-TO-QAST($context)
    }
}

# A bind (`:=`) initializer.
class RakuAST::Initializer::Bind is RakuAST::Initializer {
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Initializer::Bind, '$!expression', $expression);
        $obj
    }

    method is-binding() { True }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!expression.IMPL-TO-QAST($context)
    }
}

# A basic normal variable declaration.
class RakuAST::Declaration::Var is RakuAST::Declaration::Lexical
        is RakuAST::ImplicitLookups is RakuAST::Meta {
    has RakuAST::Type $.type;
    has str $.name;
    has RakuAST::Initializer $.initializer;

    method new(str :$name!, RakuAST::Type :$type, RakuAST::Initializer :$initializer) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::Var, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Declaration::Var, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::Declaration::Var, '$!initializer',
            $initializer // RakuAST::Initializer);
        $obj
    }

    method lexical-name() {
        $!name
    }

    method sigil() {
        nqp::substr($!name, 0, 1)
    }

    method twigil() {
        if nqp::chars($!name) > 2 {
            my str $twigil := nqp::substr($!name, 1, 1);
            nqp::index('.!^:*?=~', $twigil) >= 0 ?? $twigil !! ''
        }
        else {
            ''
        }
    }

    method visit-children(Code $visitor) {
        my $type := $!type;
        $visitor($type) if nqp::isconcrete($type);
        my $initializer := $!initializer;
        $visitor($initializer) if nqp::isconcrete($initializer);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        # Need container descriptor, even for aggregate.
        my @lookups := [
            RakuAST::Type::Simple.new('ContainerDescriptor'),
        ];

        # If it's an @ or % sigil, we need the aggergate type; if not,
        # fall back to Scalar.
        my str $sigil := self.sigil;
        if $sigil eq '@' {
            @lookups.push(RakuAST::Type::Simple.new('Array'));
        }
        elsif $sigil eq '%' {
            @lookups.push(RakuAST::Type::Simple.new('Hash'));
        }
        else {
            @lookups.push(RakuAST::Type::Simple.new('Scalar'));
        }

        # Also the type that goes inside of it.
        if $!type {
            @lookups.push($!type); # Constraint
            @lookups.push($!type); # Default
        }
        else {
            @lookups.push(RakuAST::Type::Simple.new('Mu'));
            @lookups.push(RakuAST::Type::Simple.new('Any'));
        }

        self.IMPL-WRAP-LIST(@lookups)
    }

    method PRODUCE-META-OBJECT() {
        # Form container descriptor.
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $cont-desc-type := @lookups[0].resolution.compile-time-value;
        my $of := @lookups[2].resolution.compile-time-value;
        my $default := @lookups[3].resolution.compile-time-value;
        my int $dynamic := self.twigil eq '*' ?? 1 !! 0;
        my $cont-desc := $cont-desc-type.new(:$of, :$default, :$dynamic,
            :name($!name));

        # Form the container.
        my $container-type := @lookups[1].resolution.compile-time-value;
        my $container := nqp::create($container-type);
        my str $sigil := self.sigil;
        nqp::bindattr($container, $container-type, '$!descriptor', $cont-desc);
        unless $sigil eq '@' || $sigil eq '%' {
            nqp::bindattr($container, $container-type, '$!value', $default);
        }

        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if $!initializer && $!initializer.is-binding {
            QAST::Var.new( :scope('lexical'), :decl('var'), :name($!name) )
        }
        else {
            my $container := self.meta-object;
            $context.ensure-sc($container);
            QAST::Var.new(
                :scope('lexical'), :decl('contvar'), :name($!name),
                :value($container)
            )
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $name := $!name;
        my $var-access := QAST::Var.new( :$name, :scope<lexical> );
        if $!initializer {
            my $init-qast := $!initializer.IMPL-TO-QAST($context);
            my str $sigil := self.sigil;
            if $!initializer.is-binding {
                # TODO type checking of source
                my $source := $sigil eq '@' || $sigil eq '%'
                    ?? QAST::Op.new( :op('decont'), $init-qast)
                    !! $init-qast;
                QAST::Op.new( :op('bind'), $var-access, $source )
            }
            else {
                # Assignment. Case-analyze by sigil.
                if $sigil eq '@' || $sigil eq '%' {
                    # Call STORE method.
                    nqp::die('array/hash init NYI');
                }
                else {
                    # Scalar assignment.
                    QAST::Op.new( :op('p6assign'), $var-access, $init-qast )
                }
            }
        }
        else {
            # Just a declaration; compile into an access to the variable.
            $var-access
        }
    }
}
