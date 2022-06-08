# Some kind of type (done by all kinds of things that result in a type).
class RakuAST::Type is RakuAST::Term is RakuAST::Meta {
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
}

# A simple type name, e.g. Int, Foo::Bar, etc.
class RakuAST::Type::Simple is RakuAST::Type is RakuAST::Lookup {
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
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.resolution.compile-time-value
    }
}

# A simple type name, e.g. Int, Foo::Bar, etc. that should be looked up in the
# setting.
class RakuAST::Type::Setting is RakuAST::Type::Simple {
    # TODO limit lookup to setting
}

class RakuAST::Type::Coercion is RakuAST::Type is RakuAST::Lookup {
    has RakuAST::Name $.name;
    has RakuAST::Type $!constraint;

    method new(RakuAST::Name $name, Mu $constraint) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Coercion, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Coercion, '$!constraint', $constraint);
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
        Perl6::Metamodel::CoercionHOW.new_type(
            self.resolution.compile-time-value,
            $!constraint.meta-object,
        );
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method visit-children(Code $visitor) {
        $visitor($!constraint);
    }
}

class RakuAST::Type::Definedness is RakuAST::Type is RakuAST::Lookup {
    has RakuAST::Name $.name;
    has Bool $.definite;

    method new(RakuAST::Name $name, Bool $definite) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Definedness, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Definedness, '$!definite', $definite ?? True !! False);
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
        Perl6::Metamodel::DefiniteHOW.new_type(
            :base_type(self.resolution.compile-time-value),
            :definite($!definite),
        );
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }
}

class RakuAST::Type::Capture is RakuAST::Type is RakuAST::Declaration {
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
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
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

class RakuAST::Type::Parameterized is RakuAST::Type is RakuAST::Lookup {
    has RakuAST::Name $.name;
    has RakuAST::ArgList $.args;

    method new(RakuAST::Name $name, RakuAST::ArgList $args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Parameterized, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Parameterized, '$!args', $args);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name-constant($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!args);
    }

    method PRODUCE-META-OBJECT() {
        my $role := self.resolution.compile-time-value;
        my $args := $!args.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new);
        my @pos := $args[0];
        my %named := $args[1];
        $role.HOW.parameterize($role, |@pos, |%named)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.meta-object;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() {
        self.is-resolved
            && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
            && $!args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }
}
