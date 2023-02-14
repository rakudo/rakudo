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
    is RakuAST::PackageInstaller
{
    has RakuAST::Name       $.name;
    has RakuAST::Trait::Of  $.of;
    has RakuAST::Expression $.where;
    has Mu $!current-package;
    has Mu $!block;

    method new(
      str                 :$scope,
      RakuAST::Name       :$name!,
      RakuAST::Trait::Of  :$of,
      RakuAST::Expression :$where,
      List                :$traits
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!of', $of) if $of;
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!where', $where) if $where;
        $obj.handle-traits($traits) if $traits;
        $obj
    }

    method handle-traits($traits) {
        for self.IMPL-UNWRAP-LIST($traits) {
            nqp::istype($_, RakuAST::Trait::Of)
              ?? $!of
                ?? nqp::die("Cannot declare more than one 'of' trait per subset")
                !! nqp::bindattr(self, RakuAST::Type::Subset, '$!of', $_)
              !! self.add-trait($_);
        }
    }

    method default-scope() { 'our' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our']) }

    method lexical-name() { $!name.canonicalize }

    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method attach(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Type::Subset, '$!current-package', $resolver.current-package);
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!where) if $!where;
        # External constants break if visited with missing IMPL-QAST-DECL.
        # Adding a sensible IMPL-QAST-DECL results in lexical declarations
        # for things like Int, which will break if added more than once.
        $visitor($!of)
          if $!of
          && !nqp::istype($!of, RakuAST::Declaration::External::Constant);
    }

    method is-lexical() { True }
    method is-simple-lexical-declaration() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-GENERATE-LEXICAL-DECLARATION(RakuAST::Name $name, Mu $type-object) {
        RakuAST::VarDeclaration::Implicit::Constant.new:
            :name($name),
            :value($type-object),
            :scope(self.scope);
    }

    method is-begin-performed-after-children() { True }

    method PERFORM-BEGIN-AFTER-CHILDREN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self);

        my $block := $!where;
        if $!where && !$!where.IMPL-CURRIED && (!nqp::istype($!where, RakuAST::Code) || nqp::istype($!where, RakuAST::RegexThunk)) {
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
        }
        nqp::bindattr(self, RakuAST::Type::Subset, '$!block', $block);

        # set up the meta object
        my $package := $!current-package;
        my $type    := self.stubbed-meta-object;
        $type.HOW.set_name(
          $type,
          $!name.qualified-with(
            RakuAST::Name.from-identifier-parts(
              |nqp::split('::', $package.HOW.name($package))
            )
          ).canonicalize(:colonpairs(0))
        ) unless nqp::eqaddr($package, $resolver.get-global);

        self.IMPL-INSTALL-PACKAGE(
          $resolver, self.scope, $!name, $type, $package
        );
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        Perl6::Metamodel::SubsetHOW.new_type(
            :name($!name.canonicalize),
            :refinee(Any),
            :refinement(Any)
        )
    }

    method PRODUCE-META-OBJECT() {
        my $type  := self.stubbed-meta-object;
        my $block := $!block;

        $type.HOW.set_of($type, $!of.type.PRODUCE-META-OBJECT)
          if $!of;
        $type.HOW.set_where(
          $type,
          $block.IMPL-CURRIED
            ?? $block.IMPL-CURRIED.meta-object
            !! $block.compile-time-value
        ) if $block;

        $type
    }
}
