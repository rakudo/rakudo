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

    # Generate a lookup of this variable, already resolved to this declaration.
    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method visit-children(Code $visitor) {
        my $type := $!type;
        $visitor($type) if nqp::isconcrete($type);
        my $initializer := $!initializer;
        $visitor($initializer) if nqp::isconcrete($initializer);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        # If we have a type, we need to resolve that.
        my @lookups;
        if $!type {
            @lookups.push($!type);
        }
        self.IMPL-WRAP-LIST(@lookups)
    }

    method PRODUCE-META-OBJECT() {
        # Extract implicit lookups.
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;

        # If it's a natively typed scalar, no container.
        my str $sigil := self.sigil;
        my int $is-native := nqp::objprimspec($of);
        if $sigil eq '$' && $is-native {
            return Nil;
        }

        # Form container descriptor.
        my $default := $!type ?? $of !! Any;
        my int $dynamic := self.twigil eq '*' ?? 1 !! 0;
        my $cont-desc := ContainerDescriptor.new(:$of, :$default, :$dynamic,
            :name($!name));

        # Form the container.
        my $container-base-type;
        my $container-type;
        if $sigil eq '@' {
            $container-base-type := Array;
            $container-type := $!type
                ?? Array.HOW.parameterize(Array, $of)
                !! Array;
        }
        elsif $sigil eq '%' {
            $container-base-type := Hash;
            $container-type := $!type
                ?? Hash.HOW.parameterize(Hash, $of)
                !! Hash;
        }
        else {
            $container-base-type := Scalar;
            $container-type := Scalar;
        }
        my $container := nqp::create($container-type);
        nqp::bindattr($container, $container-base-type, '$!descriptor', $cont-desc);
        unless $sigil eq '@' || $sigil eq '%' {
            nqp::bindattr($container, $container-base-type, '$!value', $default);
        }

        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
        my str $sigil := self.sigil;
        if $sigil eq '$' && nqp::objprimspec($of) {
            # Natively typed; just declare it.
            QAST::Var.new(
                :scope('lexical'), :decl('var'), :name($!name),
                :returns($of)
            )
        }
        elsif $!initializer && $!initializer.is-binding {
            # Will be bound on first use, so just a declaration.
            QAST::Var.new( :scope('lexical'), :decl('var'), :name($!name) )
        }
        else {
            # Need to vivify the object. Note: maybe we want to drop the
            # contvar, though we'll need an alternative for BEGIN.
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
        my str $sigil := self.sigil;
        my $var-access := QAST::Var.new( :$name, :scope<lexical> );
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
        if $sigil eq '$' && (my int $prim-spec := nqp::objprimspec($of)) {
            # Natively typed value. Need to initialize it to a default
            # in the absence of an initializer.
            my $init;
            if $!initializer {
                if nqp::istype($!initializer, RakuAST::Initializer::Assign) {
                    $init := $!initializer.expression.IMPL-TO-QAST($context);
                }
                else {
                    nqp::die('Can only compile an assign initializer on a native');
                }
            }
            elsif $prim-spec == 1 {
                $init := QAST::IVal.new( :value(0) );
            }
            elsif $prim-spec == 2 {
                $init := QAST::NVal.new( :value(0e0) );
            }
            else {
                $init := QAST::SVal.new( :value('') );
            }
            QAST::Op.new( :op('bind'), $var-access, $init )
        }
        else {
            # Reference type value.
            if $!initializer {
                my $init-qast := $!initializer.IMPL-TO-QAST($context);
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

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $scope := 'lexical';
        unless $rvalue {
            # Potentially l-value native lookups need a lexicalref.
            if self.sigil eq '$' {
                my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
                my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
                if nqp::objprimspec($of) {
                    $scope := 'lexicalref';
                }
            }
        }
        QAST::Var.new( :name($!name), :$scope )
    }
}
