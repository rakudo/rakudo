# Some kind of type (done by all kinds of things that result in a type).
class RakuAST::Type
  is RakuAST::Term
  is RakuAST::Meta
{
    # Checks if the type is statically known to be some particular type
    # (provided as the type object, not as another RakuAST node).
    method is-known-to-be(Mu $type) {
        nqp::die('Expected a type object') if nqp::isconcrete($type);
        if nqp::istype(self, RakuAST::Lookup) && self.is-resolved {
            my $resolution := self.resolution;
            if nqp::istype($resolution, RakuAST::CompileTimeValue) {
                return nqp::istype($resolution.compile-time-value, $type);
            }
        }
        0
    }
    method is-known-to-be-exactly(Mu $type) {
        nqp::die('Expected a type object') if nqp::isconcrete($type);
        if nqp::istype(self, RakuAST::Lookup) && self.is-resolved {
            my $resolution := self.resolution;
            if nqp::istype($resolution, RakuAST::CompileTimeValue) {
                return $resolution.compile-time-value =:= $type;
            }
        }
        0
    }
}

# A simple type name, e.g. Int, Foo::Bar, etc.
class RakuAST::Type::Simple
  is RakuAST::Type
  is RakuAST::Lookup
{
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Simple, '$!name', $name);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name-constant($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method PRODUCE-META-OBJECT() {
        self.resolution.compile-time-value
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.resolution.compile-time-value;
        if $value.HOW.archetypes.generic {
            QAST::Var.new( :name($!name.canonicalize), :scope('lexical') )
        }
        else {
            $context.ensure-sc($value);
            QAST::WVal.new( :$value )
        }
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.resolution.compile-time-value
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# A simple type name, e.g. Int, Foo::Bar, etc. that should be looked up in the
# setting.
class RakuAST::Type::Setting
  is RakuAST::Type::Simple
{
    # TODO limit lookup to setting
}

class RakuAST::Type::Derived
  is RakuAST::Type
  is RakuAST::Lookup
{
    has RakuAST::Type $.base-type;

    method resolve-with(RakuAST::Resolver $resolver) {
        $!base-type.resolve-with($resolver);
        self.set-resolution(self);
        Nil
    }
}

class RakuAST::Type::Coercion
  is RakuAST::Type::Derived
  is RakuAST::Declaration
{
    has RakuAST::Type $.constraint;

    method new(RakuAST::Type $base-type, Mu $constraint) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Coercion, '$!constraint', $constraint);
        $obj
    }

    method PRODUCE-META-OBJECT() {
        Perl6::Metamodel::CoercionHOW.new_type(
            self.base-type.compile-time-value,
            $!constraint.meta-object,
        );
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        nqp::istype(self.base-type, RakuAST::CompileTimeValue)
        && nqp::istype($!constraint, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method visit-children(Code $visitor) {
        $visitor(self.base-type);
        $visitor($!constraint);
    }

    method is-simple-lexical-declaration() {
        False
    }
}

class RakuAST::Type::Definedness
  is RakuAST::Type::Derived
  is RakuAST::Declaration
{
    has Bool $.definite;

    method new(RakuAST::Type $base-type, Bool $definite) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Definedness, '$!definite', $definite ?? True !! False);
        $obj
    }

    method PRODUCE-META-OBJECT() {
        Perl6::Metamodel::DefiniteHOW.new_type(
            :base_type(self.base-type.compile-time-value),
            :definite($!definite),
        );
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        nqp::istype(self.base-type, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method is-simple-lexical-declaration() {
        False
    }

    method visit-children(Code $visitor) {
        $visitor(self.base-type);
    }
}

class RakuAST::Type::Capture
  is RakuAST::Type
  is RakuAST::Declaration
{
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Capture, '$!name', $name);
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

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }

    method PRODUCE-META-OBJECT() {
        Perl6::Metamodel::GenericHOW.new_type(
            :name($!name.canonicalize),
        );
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name.canonicalize) )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, Mu $source-qast) {
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :scope('lexical'), :name($!name.canonicalize) ),
            QAST::Op.new(:op('what'), $source-qast)
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :name($!name.canonicalize), :scope('lexical') )
    }

    method IMPL-CAN-INTERPRET() {
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

class RakuAST::Type::Parameterized
  is RakuAST::Type::Derived
  is RakuAST::Declaration
{
    has RakuAST::ArgList $.args;

    method new(RakuAST::Type $base-type, RakuAST::ArgList $args) {
        nqp::die('need a base-type, not ' ~ $base-type.dump) if !nqp::istype($base-type, RakuAST::Type);
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Parameterized, '$!args', $args);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.base-type);
        $visitor($!args);
    }

    method PRODUCE-META-OBJECT() {
        if $!args.IMPL-HAS-ONLY-COMPILE-TIME-VALUES {
            my $args := $!args.IMPL-COMPILE-TIME-VALUES;
            my @pos := $args[0];
            my %named := $args[1];
            my $ptype := self.base-type.compile-time-value;
            $ptype.HOW.parameterize($ptype, |@pos, |%named)
        }
        else {
            nqp::die('Cannot do compile time parameterization with these args');
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        if $!args.IMPL-HAS-ONLY-COMPILE-TIME-VALUES {
            my $value := self.meta-object;
            $context.ensure-sc($value);
            QAST::WVal.new( :$value )
        }
        else {
            my $ptype := self.base-type.compile-time-value;
            my $ptref := QAST::WVal.new( :value($ptype) );
            my $qast := QAST::Op.new(:op<callmethod>, :name<parameterize>, QAST::Op.new(:op<how>, $ptref), $ptref);
            $!args.IMPL-ADD-QAST-ARGS($context, $qast);
            $qast
        }
    }

    method IMPL-CAN-INTERPRET() {
        nqp::istype(self.base-type, RakuAST::CompileTimeValue)
        && $!args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method is-simple-lexical-declaration() {
        False
    }
}

class RakuAST::Type::Subset
    is RakuAST::Type
    is RakuAST::Declaration
    is RakuAST::BeginTime
    is RakuAST::TraitTarget
    is RakuAST::StubbyMeta
    is RakuAST::Attaching
{
    has RakuAST::Name $.name;
    has RakuAST::Expression $.where;
    has RakuAST::Declaration $.of;
    has RakuAST::Package $!current-package;

    method new(RakuAST::Name :$name, RakuAST::Expression :$where, List :$traits, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!where', $where);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        for $obj.IMPL-UNWRAP-LIST($traits) {
            $obj.add-trait($_.ast);
        }
        $obj
    }

    method default-scope() { 'our' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our']) }

    method lexical-name() { $!name.canonicalize }

    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!where);
        # External constants break if visited with missing IMPL-QAST-DECL. Adding a sensible IMPL-QAST-DECL
        # results in lexical declarations for things like Int, which will break if added more than once.
        $visitor($!of) if $!of && !nqp::istype($!of, RakuAST::Declaration::External::Constant);
        # Below fails with No such method 'apply-sink' for invocant of type 'GLOBALish'
        #$visitor($!current-package);
    }

    method is-lexical() { True }
    method is-simple-lexical-declaration() { False }

    method attach(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Type::Subset, '$!current-package', $resolver.current-package);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContenxt $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-INSTALL-SUBSET(RakuAST::Resolver $resolver, RakuAST::Name $name, Mu $type-object) {
        my str $scope := self.scope;
        my $target;
        my $final;
        my $lexical;
        my $lexically-registered;
        if $name.is-identifier {
            $final := $name.canonicalize;
            $lexical := $resolver.resolve-lexical-constant($final);

            # If the subset shares the name of a stubbed package, it will have resolved via the above.
            if !$lexical {
                $resolver.current-scope.merge-generated-lexical-declaration:
                    # TODO: Using Implicit::Constant because it had the right type signature/QAST output
                    RakuAST::VarDeclaration::Implicit::Constant.new:
                        :name($final),
                        :value($type-object),
                        :scope($scope);
            }
            $lexically-registered := True;

            # If `our`-scoped, also put it into the current package.
            if $scope eq 'our' {
                # TODO conflicts
                $target := $!current-package;
            }
        } else {
            my @parts := nqp::clone(self.IMPL-UNWRAP-LIST($name.parts));
            $final := nqp::pop(@parts).name;
            my $resolved := $resolver.partially-resolve-name-constant(RakuAST::Name.new(|@parts));

            if $resolved { # first parts of the name found
                $resolved := self.IMPL-UNWRAP-LIST($resolved);
                $target := $resolved[0];
                my $parts := $resolved[1];
                my @parts := self.IMPL-UNWRAP-LIST($parts);
                $scope := 'our'; # Ensure we install the package into the parent stash
                if nqp::elems(@parts) {
                    my $longname := $target.HOW.name($target);

                    for @parts {
                        $longname := $longname ~ '::' ~ $_.name;
                        my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                        $package.HOW.compose($package);
                        my %stash := $resolver.IMPL-STASH-HASH($target);
                        %stash{$_.name} := $package;
                        $target := $package;
                    }
                }
            } else {
                my $first := nqp::shift(@parts).name;
                $target := Perl6::Metamodel::PackageHOW.new_type(name => $first);
                $target.HOW.compose($target);

                $resolver.current-scope.merge-generated-lexical-declaration:
                    RakuAST::Declaration::LexicalPackage.new:
                        :lexical-name($first),
                        :compile-time-value($target),
                        :package($!current-package);
                if $scope eq 'our' {
                    # TODO conflicts
                    my %stash := $resolver.IMPL-STASH-HASH($!current-package);
                    %stash{$first} := $target;
                }
                $scope := 'our'; # Ensure we install the package into the generated stub

                my $longname := $first;
                for @parts {
                    $longname := $longname ~ '::' ~ $_.name;
                    my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                    $package.HOW.compose($package);
                    my %stash := $resolver.IMPL-STASH-HASH($target);
                    %stash{$_.name} := $package;
                    $target := $package;
                }
            }
            $lexical := $resolver.resolve-lexical-constant($final);
        }

        my %stash := $resolver.IMPL-STASH-HASH($target);
        # upgrade a lexically imported package stub to package scope if it exists
        if $lexical {
            %stash{$final} := $lexical.compile-time-value;
        }

        # Take care of installing in case we had to make or find packages
        if !$lexically-registered {
            $resolver.current-scope.merge-generated-lexical-declaration:
                #TODO: Using Implicit::Constant because it had the right type signature/QAST output combination
                RakuAST::VarDeclaration::Implicit::Constant.new:
                    :name($final),
                    :value($type-object),
                    :scope($scope);
        }

        if $scope eq 'our' {
            if nqp::existskey(%stash, $final) {
                nqp::setwho($type-object, %stash{$final}.WHO);
            }
            %stash{$final} := $type-object;
        }
    }

    method is-begin-performed-after-children() { True }

    method PERFORM-BEGIN-AFTER-CHILDREN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self);

        for self.IMPL-UNWRAP-LIST(self.traits) {
            if nqp::istype($_, RakuAST::Trait::Of) {
                if $!of {
                    nqp::die("Cannot declare more than one 'of' trait per subset");
                }
                my $of-type := $resolver.resolve-name-constant($_.type.name);
                nqp::bindattr(self, RakuAST::Type::Subset, '$!of', $of-type);
            }
        }

        my $block;
        if !$!where.IMPL-CURRIED && (!nqp::istype($!where, RakuAST::Code) || nqp::istype($!where, RakuAST::RegexThunk)) {
            $block := RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyPostfix.new(
                                operand => RakuAST::ApplyPostfix.new(
                                    operand => $!where,
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('ACCEPTS'),
                                        args => RakuAST::ArgList.new(
                                            RakuAST::Var::Lexical.new('$_'),
                                        ),
                                    ),
                                ),
                                postfix => RakuAST::Call::Method.new(
                                    name => RakuAST::Name.from-identifier('Bool'),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            $block.IMPL-CHECK($resolver, $context, False);
            nqp::bindattr(self, RakuAST::Type::Subset, '$!where', $block);
        }

        my $type-object := self.stubbed-meta-object;
        $type-object.HOW.set_name(
            $type-object,
            $!name.qualified-with(
                RakuAST::Name.from-identifier-parts(
                    |nqp::split('::', $!current-package.HOW.name($!current-package))
                )
            ).canonicalize(:colonpairs(0))
        ) if !nqp::eqaddr($!current-package, $resolver.get-global);

        self.IMPL-INSTALL-SUBSET($resolver, $!name, $type-object);
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        Perl6::Metamodel::SubsetHOW.new_type(
            :name($!name.canonicalize),
            :refinee(Any),
            :refinement(Any)
        )
    }

    method PRODUCE-META-OBJECT() {
        my $type := self.stubbed-meta-object;
        if $!of {
            $type.HOW.set_of($type, $!of.compile-time-value);
        }
        $type.HOW.set_where(
            $type,
            $!where.IMPL-CURRIED
                ?? $!where.IMPL-CURRIED.meta-object
                !! $!where.compile-time-value
        );
        $type
    }
}