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

class RakuAST::ContainerCreator {
    method IMPL-CONTAINER(Mu $of) {
        # If it's a natively typed scalar, no container.
        my str $sigil := self.sigil;
        my int $is-native := nqp::objprimspec($of);
        if $sigil eq '$' && $is-native {
            return Nil;
        }

        # Form container descriptor.
        my $default := self.type ?? $of !! Any;
        my int $dynamic := self.twigil eq '*' ?? 1 !! 0;
        my $cont-desc := ContainerDescriptor.new(:$of, :$default, :$dynamic,
            :name(self.lexical-name));

        # Form the container.
        my $container-base-type;
        my $container-type;
        if $sigil eq '@' {
            $container-base-type := Array;
            $container-type := self.type
                ?? Array.HOW.parameterize(Array, $of)
                !! Array;
        }
        elsif $sigil eq '%' {
            $container-base-type := Hash;
            $container-type := self.type
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
}

# A basic variable declaration of the form `my SomeType $foo = 42` or `has Foo $x .= new`.
class RakuAST::VarDeclaration::Simple is RakuAST::Declaration is RakuAST::ImplicitLookups
                                      is RakuAST::TraitTarget is RakuAST::ContainerCreator
                                      is RakuAST::Meta is RakuAST::Attaching {
    has RakuAST::Type $.type;
    has str $.name;
    has str $!storage-name;
    has RakuAST::Initializer $.initializer;
    has RakuAST::Package $!attribute-package;
    has Mu $!package;

    method new(str :$name!, RakuAST::Type :$type, RakuAST::Initializer :$initializer,
               str :$scope) {
        my $obj := nqp::create(self);
        if nqp::chars($name) < 2 {
            nqp::die('Cannot use RakuAST::VarDeclaration::Simple to declare an anonymous varialbe; use RakuAST::VarDeclaration::Anonymous');
        }
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!initializer',
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

    method desigilname() {
        nqp::substr($!name, self.twigil ?? 2 !! 1)
    }

    # Generate a lookup of this variable, already resolved to this declaration.
    method generate-lookup() {
        if self.is-lexical {
            my $lookup := RakuAST::Var::Lexical.new($!name);
            $lookup.set-resolution(self);
            $lookup
        }
        else {
            nqp::die('Cannot generate lookup of simple var for scope ' ~ self.scope);
        }
    }

    method can-be-bound-to() {
        # Must be lexical and non-native.
        if self.scope eq 'my' {
            my str $sigil := self.sigil;
            return True if $sigil eq '@' || $sigil eq '%';
            return True unless $!type;
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
            return True unless nqp::objprimspec(@lookups[0].resolution.compile-time-value);
        }
        False
    }

    method visit-children(Code $visitor) {
        my $type := $!type;
        $visitor($type) if nqp::isconcrete($type);
        my $initializer := $!initializer;
        $visitor($initializer) if nqp::isconcrete($initializer);
    }

    method default-scope() {
        'my'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'state', 'our', 'has', 'HAS'])
    }

    method is-lexical() {
        # Overridden here because our-scoped variables are really lexical aliases.
        my str $scope := self.scope;
        $scope eq 'my' || $scope eq 'state' || $scope eq 'our'
    }

    method attach(RakuAST::Resolver $resolver) {
        my str $scope := self.scope;
        if $scope eq 'has' || $scope eq 'HAS' {
            my $attribute-package := $resolver.find-attach-target('package');
            if $attribute-package {
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!attribute-package',
                    $attribute-package);
                $attribute-package.ATTACH-ATTRIBUTE(self);
            }
            else {
                # TODO check-time error
            }
        }
        elsif $scope eq 'our' {
            my $package := $resolver.current-package;
            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!package',
                $package);
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        # If it's our-scoped, we need the package to bind it from.
        my str $scope := self.scope;
        if $scope eq 'our' {
            @lookups.push(RakuAST::Var::Compiler::Lookup.new('$?PACKAGE'));
        }
        # If we have a type, we need to resolve that.
        elsif $!type {
            @lookups.push($!type);
        }
        # If we're has/HAS scope, we need Nil to evaluate to.
        if $scope eq 'has' || $scope eq 'HAS' {
            @lookups.push(RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')));
        }
        self.IMPL-WRAP-LIST(@lookups)
    }

    method PRODUCE-META-OBJECT() {
        # If it's our-scoped, then container is vivified via. package access.
        my str $scope := self.scope;
        return Nil if $scope eq 'our';

        # Calculate the type.
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;

        # If it's has scoped, we'll need to build an attribute.
        if $scope eq 'has' || $scope eq 'HAS' {
            $!attribute-package.attribute-type.new(
                name => self.sigil ~ '!' ~ self.desigilname,
                type => $of,
                has_accessor => self.twigil eq '.',
                auto_viv_container => self.IMPL-CONTAINER($of),
                package => $!attribute-package.compile-time-value
            )
        }

        # Otherwise, it's lexically scoped, so the meta-object is just the
        # container, if any.
        else {
            self.IMPL-CONTAINER($of)
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my str $scope := self.scope;
        if $scope eq 'my' {
            # Lexically scoped
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
        elsif $scope eq 'our' {
            # Package scoped lexical alias. We want to bind the lexical to a lookup
            # in the package.
            my $name := RakuAST::Name.from-identifier($!name);
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :scope('lexical'), :decl('var'), :name($!name) ),
                $name.IMPL-QAST-PACKAGE-LOOKUP($context, QAST::WVal.new(:value($!package)))
            )
        }
        elsif $scope eq 'has' || $scope eq 'HAS' {
            # No declaration to install
            QAST::Op.new( :op('null') )
        }
        elsif $scope eq 'state' {
            # Lexically scoped state variable
            my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
            my str $sigil := self.sigil;
            if $sigil eq '$' && nqp::objprimspec($of) {
                nqp::die("Natively typed state variables not yet implemented");
            }
            elsif $!initializer && $!initializer.is-binding {
                # Will be bound on first use, so just a declaration.
                QAST::Var.new(:scope('lexical'), :decl('var'), :name($!name))
            }
            else {
                # Need to vivify the object.
                my $container := self.meta-object;
                $context.ensure-sc($container);
                QAST::Var.new(
                    :scope('lexical'), :decl('statevar'), :name($!name),
                    :value($container)
                )
            }
        }
        else {
            nqp::die("Don't know how to compile $scope scope variable");
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $scope := self.scope;
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            my str $name := $!name;
            my str $sigil := self.sigil;
            my $var-access := QAST::Var.new( :$name, :scope<lexical> );
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
                    my $perform-init-qast;
                    if $!initializer.is-binding {
                        # TODO type checking of source
                        my $source := $sigil eq '@' || $sigil eq '%'
                            ?? QAST::Op.new( :op('decont'), $init-qast)
                            !! $init-qast;
                        $perform-init-qast := QAST::Op.new( :op('bind'), $var-access, $source );
                    }
                    else {
                        # Assignment. Case-analyze by sigil.
                        if $sigil eq '@' || $sigil eq '%' {
                            # Call STORE method, passing :INITIALIZE to indicate it's
                            # the initialization for immutable types.
                            $perform-init-qast := QAST::Op.new(
                                :op('callmethod'), :name('STORE'),
                                $var-access,
                                $init-qast,
                                QAST::WVal.new( :named('INITIALIZE'), :value(True) )
                            );
                        }
                        else {
                            # Scalar assignment.
                            $perform-init-qast := QAST::Op.new(
                                :op('p6assign'), $var-access, $init-qast );
                        }
                    }
                    if $scope eq 'state' {
                        QAST::Op.new(
                            :op('if'),
                            QAST::Op.new( :op('p6stateinit') ),
                            $perform-init-qast,
                            $var-access
                        )
                    }
                    else {
                        $perform-init-qast
                    }
                }
                else {
                    # Just a declaration; compile into an access to the variable.
                    $var-access
                }
            }
        }
        elsif $scope eq 'has' || $scope eq 'HAS' {
            # These just evaluate to Nil
            @lookups[nqp::elems(@lookups) - 1].IMPL-TO-QAST($context)
        }
        else {
            nqp::die("Don't know how to compile initialization for scope $scope");
        }
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $scope := self.scope;
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            my str $scope := 'lexical';
            unless $rvalue {
                # Potentially l-value native lookups need a lexicalref.
                if self.sigil eq '$' && self.scope ne 'our' {
                    my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
                    my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
                    if nqp::objprimspec($of) {
                        $scope := 'lexicalref';
                    }
                    return QAST::Var.new( :name($!name), :$scope, :returns($of) );
                }
            }
            QAST::Var.new( :name($!name), :$scope )
        }
        elsif $scope eq 'has' || $scope eq 'HAS' {
            nqp::die('Cannot compile lookup of attributes yet')
        }
        else {
            nqp::die("Cannot compile lookup of scope $scope")
        }
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, RakuAST::Expression $source) {
        my str $scope := self.scope;
        nqp::die('Can only compile bind to my-scoped variables') unless $scope eq 'my';
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($!name), :scope('lexical') ),
            $source.IMPL-TO-QAST($context)
        )
    }

    method needs-sink-call() { False }
}

class RakuAST::VarDeclaration::Signature is RakuAST::Declaration is RakuAST::ImplicitLookups
                                      is RakuAST::TraitTarget is RakuAST::CheckTime
                                      is RakuAST::Attaching {
    has RakuAST::Signature $.signature;
    has RakuAST::Type $.type;
    has RakuAST::Initializer $.initializer;
    has RakuAST::Package $!attribute-package;
    has Mu $!package;

    method new(RakuAST::Signature :$signature!, RakuAST::Type :$type, RakuAST::Initializer :$initializer,
               str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!signature', $signature);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!initializer',
            $initializer // RakuAST::Initializer);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        my $type := $!type;
        $visitor($type) if nqp::isconcrete($type);
        my $initializer := $!initializer;
        $visitor($initializer) if nqp::isconcrete($initializer);
    }

    method default-scope() {
        'my'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'state', 'our', 'has', 'HAS'])
    }

    method is-simple-lexical-declaration() {
        # The ParameterTargets in our signature will take care of declarations
        False
    }

    method is-lexical() {
        # Overridden here because our-scoped variables are really lexical aliases.
        my str $scope := self.scope;
        $scope eq 'my' || $scope eq 'state' || $scope eq 'our'
    }

    method attach(RakuAST::Resolver $resolver) {
        my str $scope := self.scope;
        if $scope eq 'has' || $scope eq 'HAS' {
            my $attribute-package := $resolver.find-attach-target('package');
            if $attribute-package {
                nqp::bindattr(self, RakuAST::VarDeclaration::Signature, '$!attribute-package',
                    $attribute-package);
                $attribute-package.ATTACH-ATTRIBUTE(self);
            }
            else {
                # TODO check-time error
            }
        }
        elsif $scope eq 'our' {
            my $package := $resolver.current-package;
            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(self, RakuAST::VarDeclaration::Signature, '$!package',
                $package);
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        # If it's our-scoped, we need the package to bind it from.
        my str $scope := self.scope;
        if $scope eq 'our' {
            @lookups.push(RakuAST::Var::Compiler::Lookup.new('$?PACKAGE'));
        }
        # If we have a type, we need to resolve that.
        elsif $!type {
            @lookups.push($!type);
        }
        # If we're has/HAS scope, we need Nil to evaluate to.
        if $scope eq 'has' || $scope eq 'HAS' {
            @lookups.push(RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')));
        }
        self.IMPL-WRAP-LIST(@lookups)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver) {
        # tell the parameter targets to create containers
        my @params := self.IMPL-UNWRAP-LIST($!signature.parameters);

        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $of := $!type ?? @lookups[0].resolution.compile-time-value !! Mu;
        my $type := $!type // RakuAST::Type::Setting.new('Mu');

        for @params {
            $_.target.set-container-type($type, $of);
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::isconcrete($!initializer) {
            if nqp::istype($!initializer, RakuAST::Initializer::Assign) {
                my $list   := QAST::Op.new( :op('call'), :name('&infix:<,>') );
                my @params := self.IMPL-UNWRAP-LIST($!signature.parameters);

                for @params {
                    $list.push: $_.target.IMPL-LOOKUP-QAST($context);
                }
                my $init-qast := $!initializer.IMPL-TO-QAST($context);
                $list := QAST::Op.new( :op('p6store'), $list, $init-qast);
                return $list;
            }
            else {
                nqp::die('Not yet supported signature initializer: ' ~ $!initializer.HOW.name($!initializer));
            }
        }

        QAST::Op.new(:op<null>);
    }

    method needs-sink-call() { False }
}

# An anonymous variable declaration, such as `my $ = 42`
class RakuAST::VarDeclaration::Anonymous is RakuAST::VarDeclaration::Simple {
    has str $.sigil;

    method new(str :$sigil!, RakuAST::Type :$type, RakuAST::Initializer :$initializer,
               str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!name',
            self.IMPL-GENERATE-NAME());
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Anonymous, '$!sigil', $sigil);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!initializer',
            $initializer // RakuAST::Initializer);
        $obj
    }

    method IMPL-GENERATE-NAME() {
        QAST::Node.unique('ANON_VAR')
    }

    method sigil() {
        $!sigil
    }

    method twigil() {
        ''
    }

    method desigilname() {
        ''
    }

    method generate-lookup() {
        nqp::die('Cannot generate lookup of an anonymous variable');
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'state'])
    }
}

# The declaration of a term (sigilless) variable.
class RakuAST::VarDeclaration::Term is RakuAST::Declaration {
    has RakuAST::Type $.type;
    has RakuAST::Name $.name;
    has RakuAST::Initializer $.initializer;

    method new(str :$scope, RakuAST::Type :$type, RakuAST::Name :$name!,
            RakuAST::Initializer :$initializer!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!initializer',
            $initializer // RakuAST::Initializer);
        $obj
    }

    method lexical-name() {
        $!name.canonicalize
    }

    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name.canonicalize) )
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $init-qast := $!initializer.IMPL-TO-QAST($context);
        if $!type {
            $init-qast := QAST::Op.new(
                :op('p6bindassert'),
                $init-qast,
                $!type.IMPL-TO-QAST($context)
            );
        }
        QAST::Op.new( :op('bind'), self.IMPL-LOOKUP-QAST($context), $init-qast )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :name($!name.canonicalize), :scope('lexical') )
    }

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }

    method needs-sink-call() { False }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
        $visitor($!name);
        $visitor($!initializer) if $!initializer;
    }
}

# The commonalities for implicitly declared variables.
class RakuAST::VarDeclaration::Implicit is RakuAST::Declaration {
    has str $.name;

    method new(str :$name!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        $obj
    }

    method lexical-name() {
        $!name
    }

    method default-scope() {
        'my'
    }

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        QAST::Var.new( :name($!name), :scope('lexical') )
    }
}

# An implicitly declared special variable. Typically used for $/, $!, and $_ in
# routines.
class RakuAST::VarDeclaration::Implicit::Special is RakuAST::VarDeclaration::Implicit
                                                 is RakuAST::Meta {
    method PRODUCE-META-OBJECT() {
        # Reuse the container descriptor for the common cases that we expect
        # to have.
        my constant COMMON := nqp::hash(
            '$_', ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :name('$_')),
            '$/', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
            '$!', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
        );
        my $cont-desc := COMMON{self.name} //
            ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :name(self.name));
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $cont-desc);
        nqp::bindattr($container, Scalar, '$!value', Any);
        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new(
            :scope('lexical'), :decl('contvar'), :name(self.name),
            :value($container)
        )
    }
}

# Implicit block topic declaration. By default it is an optional parameter,
# but can be configured to be either a non-parameter (always from outer), a
# required parameter, or to obtain the current exception (for when it is a
# CATCH or CONTROL block);
class RakuAST::VarDeclaration::Implicit::BlockTopic is RakuAST::VarDeclaration::Implicit {
    has Bool $.parameter;
    has Bool $.required;
    has Bool $.exception;

    method new(Bool :$parameter, Bool :$required, Bool :$exception) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '$_');
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!parameter',
            $parameter // True);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!required',
            $required // False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!exception',
            $exception // False);
        $obj
    }

    method set-parameter(Bool $parameter) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!parameter',
            $parameter);
        Nil
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if $!exception {
            QAST::Stmts.new(
                QAST::Var.new( :decl('param'), :scope('local'), :name('EXCEPTION') ),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :decl('var'), :scope('lexical'), :name('$_') ),
                    QAST::Op.new(
                        :op('call'), :name('&EXCEPTION'),
                        QAST::Var.new( :scope('local'), :name('EXCEPTION') )
                    )
                ),
                QAST::Op.new(
                    :op('p6assign'),
                    QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$!') ) ),
                    QAST::Var.new( :scope('lexical'), :name('$_') )
                )
            )
        }
        elsif $!parameter {
            my $param := QAST::Var.new( :decl('param'), :scope('lexical'), :name('$_') );
            unless $!required {
                $param.default(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ));
            }
            $param
        }
        else {
            QAST::Op.new:
                :op('bind'),
                QAST::Var.new( :decl('var'), :scope('lexical'), :name('$_') ),
                QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
        }
    }
}

# An implicitly declared constant variable - that is, one with a value that is
# fixed at compile time. Used for $?PACKAGE and similar.
class RakuAST::VarDeclaration::Implicit::Constant is RakuAST::VarDeclaration::Implicit
                                                  is RakuAST::CompileTimeValue {
    has Mu $.value;

    method new(str :$name!, Mu :$value!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        $obj
    }

    method compile-time-value() { $!value }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $context.ensure-sc($!value);
        QAST::Var.new( :decl('static'), :scope('lexical'), :name(self.name), :value($!value) )
    }
}

# An implicitly declared block (like an auto-generated proto)
class RakuAST::VarDeclaration::Implicit::Block is RakuAST::VarDeclaration::Implicit {
    has Mu $.block;

    method new(str :$name!, Mu :$block!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Block, '$!block', $block);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $!block.IMPL-QAST-DECL-CODE($context);
    }
}

# The implicit `self` term declaration for the invocant.
class RakuAST::VarDeclaration::Implicit::Self is RakuAST::VarDeclaration::Implicit {
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', 'self');
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) )
    }
}

# The implicit `$¢` declaration for the cursor.
class RakuAST::VarDeclaration::Implicit::Cursor is RakuAST::VarDeclaration::Implicit {
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '$¢');
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) )
    }
}

# The commonalities for placeholder parameters.
class RakuAST::VarDeclaration::Placeholder is RakuAST::Declaration is RakuAST::Attaching {
    method lexical-name() { nqp::die('Missing lexical-name implementation') }

    method generate-parameter() {
        nqp::die('Missing generate-parameter implementation')
    }

    method attach(RakuAST::Resolver $resolver) {
        my $owner := $resolver.find-attach-target('block');
        if $owner {
            $owner.add-placeholder-parameter(self);
        }
    }

    method default-scope() { 'my' }

    method allowed-scopes() { ['my'] }

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new(self.lexical-name);
        $lookup.set-resolution(self);
        $lookup
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        QAST::Var.new( :name(self.lexical-name), :scope('lexical') )
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.lexical-name) )
    }
}

# A positional placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::Positional is RakuAST::VarDeclaration::Placeholder {
    has str $.lexical-name;

    method new(str $declared-name) {
        if nqp::substr($declared-name, 1, 1) eq '^' {
            nqp::die('Should construct a ' ~ self.HOW.name(self) ~ ' without the ^ twigil');
        }
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Placeholder::Positional,
            '$!lexical-name', $declared-name);
        $obj
    }

    method generate-parameter() {
        RakuAST::Parameter.new:
            target => RakuAST::ParameterTarget::Var.new(self.lexical-name)
    }
}

# A named placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::Named is RakuAST::VarDeclaration::Placeholder {
    has str $.lexical-name;

    method new(str $declared-name) {
        if nqp::substr($declared-name, 1, 1) eq ':' {
            nqp::die('Should construct a ' ~ self.HOW.name(self) ~ ' without the : twigil');
        }
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Placeholder::Named,
            '$!lexical-name', $declared-name);
        $obj
    }

    method generate-parameter() {
        RakuAST::Parameter.new:
            target => RakuAST::ParameterTarget::Var.new(self.lexical-name),
            names => [nqp::substr(self.lexical-name, 1)],
            optional => 0
    }
}

# A slurpy array placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyArray is RakuAST::VarDeclaration::Placeholder {
    method lexical-name() { '@_' }

    method generate-parameter() {
        RakuAST::Parameter.new:
            target => RakuAST::ParameterTarget::Var.new(self.lexical-name),
            slurpy => RakuAST::Parameter::Slurpy::Flattened
    }
}

# A slurpy hash placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyHash is RakuAST::VarDeclaration::Placeholder {
    method lexical-name() { '%_' }

    method generate-parameter() {
        RakuAST::Parameter.new:
            target => RakuAST::ParameterTarget::Var.new(self.lexical-name),
            slurpy => RakuAST::Parameter::Slurpy::Flattened
    }
}
