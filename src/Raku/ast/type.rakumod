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

    method dba() { 'type' }

    method IMPL-BASE-TYPE() {
        self
    }

    method IMPL-TARGET-TYPE() {
        self
    }

    # The type to use for e.g. default values, i.e. Int on a Int:D constrainted variable.
    # Or Foo[Int] on Foo:D[Int](Bar)
    method IMPL-VALUE-TYPE() {
        self
    }

    method IMPL-MAYBE-DEFINITE-HOW-BASE($v) {
        # returns the value itself, unless it's a DefiniteHOW, in which case,
        # it returns its base type. Behaviour available in 6.d and later only.
        nqp::getcomp('Raku').language_revision >= 2
            && nqp::eqaddr($v.HOW, Perl6::Metamodel::DefiniteHOW)
            ?? $v.HOW.base_type: $v
            !! $v
    }

    method IMPL-MAYBE-NOMINALIZE($v) {
        # If type does LanguageRevision then check what language it was created with. Otherwise base decision on the
        # current compiler.
        my $v-how := $v.HOW;
        !$v-how.archetypes($v).coercive
            && (nqp::can($v-how, 'language_revision')
                    ?? $v-how.language_revision($v) < 3
                    !! nqp::getcomp('Raku').language_revision < 3)
            ?? self.IMPL-MAYBE-DEFINITE-HOW-BASE($v)
            !! ($v-how.archetypes($v).nominalizable
                ?? $v-how.nominalize($v)
                !! $v)
    }

    method is-native() { False }
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

    # This probably needs a better heuristic or be implemented as an attribute
    method is-native() {
        my str $name := $!name.canonicalize;
        nqp::lc($name) eq $name
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# A simple type name, e.g. Int, IO::Path, etc. that should be looked up in the
# setting.
class RakuAST::Type::Setting
  is RakuAST::Type::Simple
{
    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name-constant-in-setting(self.name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }
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

    method IMPL-BASE-TYPE() {
        nqp::istype($!base-type, RakuAST::Type::Derived) ?? $!base-type.IMPL-BASE-TYPE !! $!base-type
    }
}

class RakuAST::Type::Coercion
  is RakuAST::Type::Derived
  is RakuAST::Declaration
{
    has RakuAST::Type $.constraint;

    method new(RakuAST::Type :$base-type!, Mu :$constraint) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Coercion, '$!constraint',
          $constraint // RakuAST::Type::Setting.new(
            RakuAST::Name.from-identifier("Any")
          )
        );
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

    method IMPL-TARGET-TYPE() {
        my $base-type := self.base-type;
        nqp::istype($base-type, RakuAST::Type::Coercion) ?? $base-type.IMPL-TARGET-TYPE !! $base-type
    }

    method IMPL-VALUE-TYPE() {
        self.IMPL-TARGET-TYPE.IMPL-VALUE-TYPE
    }
}

class RakuAST::Type::Definedness
  is RakuAST::Type::Derived
  is RakuAST::Declaration
{
    has Bool $.definite;
    has Bool $.through-pragma;

    method new(
      RakuAST::Type :$base-type!,
               Bool :$definite!,
               Bool :$through-pragma
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Definedness, '$!definite',
          $definite ?? True !! False);
        nqp::bindattr($obj, RakuAST::Type::Definedness, '$!through-pragma',
          $through-pragma ?? True !! False);
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

    method IMPL-VALUE-TYPE() {
        self.base-type
    }

    method is-simple-lexical-declaration() {
        False
    }

    method visit-children(Code $visitor) {
        $visitor(self.base-type.IMPL-VALUE-TYPE);
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
        QAST::Var.new( :decl('static'), :scope('lexical'), :name($!name.canonicalize) )
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

    method new(RakuAST::Type :$base-type!, RakuAST::ArgList :$args) {
        nqp::die('need a base-type, not ' ~ $base-type.dump) if !nqp::istype($base-type, RakuAST::Type);
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Type::Derived, '$!base-type', $base-type);
        nqp::bindattr($obj, RakuAST::Type::Parameterized, '$!args',
          $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.base-type);
        $visitor($!args);
    }

    method PRODUCE-META-OBJECT() {
        if !$!args.args {
            self.base-type.compile-time-value
        }
        elsif $!args.IMPL-HAS-ONLY-COMPILE-TIME-VALUES {
            my $args := $!args.IMPL-COMPILE-TIME-VALUES;
            my @pos := $args[0];
            my %named := $args[1];
            my $ptype := self.IMPL-BASE-TYPE.compile-time-value;
            $ptype.HOW.parameterize($ptype, |@pos, |%named)
        }
        else {
            my $args := $!args.args;
            if nqp::istype($args.AT-POS(0), RakuAST::QuotedString) {
                my int $is-only-quoted-string;
                my int $arg-count;
                for self.IMPL-UNWRAP-LIST($args) {
                    $is-only-quoted-string := nqp::istype($_, RakuAST::QuotedString);
                    ++$arg-count;
                    last unless $is-only-quoted-string;
                }
                if $is-only-quoted-string {
                    my @literals;
                    for self.IMPL-UNWRAP-LIST($args) {
                        my $literal := $_.literal-value;
                        if nqp::isconcrete($literal) {
                            @literals.push: $literal;
                        }
                    }
                    unless nqp::elems(@literals) == $arg-count {
                        nqp::die('Not all RakuAST::QuotedString objects have literal values');
                    }
                    my $ptype := self.IMPL-BASE-TYPE.compile-time-value;
                    $ptype.HOW.parameterize($ptype, |@literals);
                }
            }
            else {
                nqp::die('Cannot do compile time parameterization with these args');
            }
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        if !$!args.args {
            QAST::WVal.new( :value(self.base-type.compile-time-value) )
        }
        elsif $!args.IMPL-HAS-ONLY-COMPILE-TIME-VALUES {
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

    method IMPL-VALUE-TYPE() {
        RakuAST::Type::Parameterized.new(:base-type(self.base-type.IMPL-VALUE-TYPE), :args($!args))
    }

    method is-simple-lexical-declaration() {
        False
    }
}

class RakuAST::Type::Enum
  is RakuAST::Type
  is RakuAST::Declaration
  is RakuAST::BeginTime
  is RakuAST::TraitTarget
  is RakuAST::Attaching
  is RakuAST::PackageInstaller
  is RakuAST::ImplicitLookups
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Name       $.name;
    has RakuAST::Expression $.term;
    has RakuAST::Type       $.of;
    has Mu                  $!current-package;
    # Note: Not using RakuAST::Type::Derived because we don't always know
    # the base-type ahead of time
    has Mu                  $!base-type;

    method new(          str :$scope,
               RakuAST::Name :$name,
               RakuAST::Type :$of,
                        List :$traits,
         RakuAST::Expression :$term!,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Type::Enum, '$!name',
          $name // RakuAST::Name.from-identifier(''));
        nqp::bindattr($obj, RakuAST::Type::Enum, '$!of', $of);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Type::Enum, '$!term', $term);
        $obj.set-WHY($WHY);
        $obj
    }

    method default-scope() { 'our' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our']) }

    method dba() { 'enum' }

    method lexical-name() { $!name.canonicalize }

    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!term);
        $visitor($!of)     if $!of;
        $visitor(self.WHY) if self.WHY;
    }

    method attach(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Type::Enum, '$!current-package', $resolver.current-package);
    }

    method is-lexical() { True }
    method is-simple-lexical-declaration() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := QAST::Op.new(:op('call'), :name('&ENUM_VALUES'), $!term.IMPL-EXPR-QAST($context));
        QAST::Want.new(
            $qast,
            'v',
            QAST::Op.new(:op('null'))
        )
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('List')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Stringy')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Numeric'))
        ])
    }

    method IMPL-GENERATE-LEXICAL-DECLARATION(RakuAST::Name $name, Mu $type-object) {
        RakuAST::VarDeclaration::Implicit::Constant.new:
            :name($name),
            :value(nqp::eqaddr($type-object, Mu) ?? self.stubbed-meta-object !! $type-object),
            :scope(self.scope);
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $lookups := self.get-implicit-lookups;
        my $Pair    := $lookups.AT-POS(0).resolution.compile-time-value;
        my $List    := $lookups.AT-POS(1).resolution.compile-time-value;
        my $Stringy := $lookups.AT-POS(2).resolution.compile-time-value;
        my $Numeric := $lookups.AT-POS(3).resolution.compile-time-value;

        my $base-type;
        my $has-base-type := False;
        if $!of {
            $base-type := $!of.compile-time-value;
            $has-base-type := True;
        }
        my %values := nqp::hash;
        my $cur-val := nqp::box_i(-1, Int); # Boxed to support .succ
        my $evaluated := self.IMPL-BEGIN-TIME-EVALUATE($!term, $resolver, $context);
        my $is-settings-list := nqp::istype($evaluated, $List);
        if nqp::istype($evaluated, $Pair) {
            if !$has-base-type {
                # No need for type checking when we are going to get the base-type from the value
                %values{$evaluated.key} := $evaluated.value;
                $base-type := $evaluated.value.WHAT;
            } else {
                unless nqp::istype($evaluated.value, $!base-type) {
                    nqp::die("Incorrect value type provided. Expected '" ~ $!base-type.raku ~ "' but got '" ~ $evaluated.value.WHAT.raku ~ "'");
                }
                %values{$evaluated.key} := $evaluated.value;
            }
        } elsif nqp::istype($evaluated, Str) {
            # TODO: What do we actually want to do when base-type is defined but they only provide a single Str?
            #       Base just ignores and uses Int
            # A single string enum will always have 0, but we use $cur-val to keep it boxed
            %values{$evaluated} := $cur-val.succ;
            $base-type := Int;
        } elsif nqp::istype($evaluated, List) || $is-settings-list {
            my @items := self.IMPL-UNWRAP-LIST($evaluated);
            if nqp::elems(@items) == 0 {
                # For empty enums, just default to Int
                $base-type := Int;
            } else {
                for @items {
                    if nqp::istype($_, $Pair) {
                        $cur-val := $_.value;
                        if !$has-base-type {
                            $base-type := $cur-val.WHAT;
                            $has-base-type := True;
                        } else {
                            # Should be a panic or a throw, right?
                            unless nqp::istype($cur-val, $!base-type) {
                                nqp::die("Incorrect value type provided. Expected '" ~ $!base-type.raku ~ "' but got '" ~ $cur-val.WHAT.raku ~ "'");
                            }
                        }
                        %values{$_.key} := $cur-val;
                    } elsif nqp::istype($_, Str) {
                        if !$has-base-type {
                            # TODO: Again, uncertain what to do when user provides a base type but then only hands a list of Str
                            $base-type := Int;
                            $has-base-type := True;
                        }
                        %values{$_} := ($cur-val := $cur-val.succ);
                    }
                }
            }
        }

        # Make $!base-type available, then we can produce the meta-object and add and apply traits
        nqp::bindattr(self, RakuAST::Type::Enum, '$!base-type', $base-type);
        my $meta := self.meta-object;
        my $enumeration-kind;
        if nqp::istype($meta, $Numeric) {
            $enumeration-kind := nqp::istype($meta, $Stringy)
                ?? 'NumericStringyEnumeration' # allomorphs
                !! 'NumericEnumeration';
        } elsif nqp::istype($meta, $Stringy) {
            $enumeration-kind := 'StringyEnumeration';
        }
        self.add-trait(RakuAST::Trait::Does.new(
            RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Enumeration'))
        ));
        if $enumeration-kind {
            self.add-trait(RakuAST::Trait::Does.new(
                RakuAST::Type::Simple.new(RakuAST::Name.from-identifier($enumeration-kind))
            ));
        }
        self.apply-traits($resolver, $context, self);
        $meta.HOW.compose($meta);

        # Don't install an anonymous enum
        my $anonymous := !$!name.canonicalize;
        if !$anonymous {
            self.IMPL-INSTALL-PACKAGE(
                $resolver, self.scope, $!name, $!current-package, :meta-object(Mu)
            );
        }

        # Create type objects for each value and install into proper scop
        my %stash := $resolver.IMPL-STASH-HASH($anonymous ?? $!current-package !! $meta);
        my int $index;
        for %values -> $pair {
            my $key     := $pair.key;
            my $value   := $pair.value;

            if !nqp::defined($value) {
                nqp::die("Using a type object as a value for an enum not yet implemented. Sorry.");
            }

            my $val-meta := nqp::rebless(nqp::clone($value), $meta);
            nqp::bindattr($val-meta, $meta, '$!key', $key);
            nqp::bindattr($val-meta, $meta, '$!value', $value);
            nqp::bindattr_i($val-meta, $meta, '$!index', $index++);
            $context.ensure-sc($val-meta);
            $meta.HOW.add_enum_value($meta, $val-meta);

            # Make sure it is not already defined, eg 'enum Day<Mon Mon>' or 'class Day::Foo {}; enum Day<Mon Foo>'
            # TODO: Base allows both. First raises a 'Potential Difficulties', second succeeds silently.
            #   But perhaps 6.e and moving forward, we could make the logic below the default behavior.
#            if nqp::existskey(%stash, $key) {
#                nqp::die("Redeclaration of symbol '" ~ $key ~ "'.");
#            }
            unless $anonymous && self.scope eq 'my' {
                %stash{$key} := $val-meta;
            }

            # Declare these values into the lexical scope
            # TODO: Bind an X::PoisonedAlias when a lexical already exists
            #   (Which is tricky, because base only does it when there is a clash in the current lexpad...)
            $resolver.current-scope.add-generated-lexical-declaration:
                RakuAST::VarDeclaration::Implicit::Constant.new(
                    :name($key),
                    :scope(self.scope),
                    :value($val-meta)
                );
        }
        $meta.HOW.compose_values($meta);
    }

    method PRODUCE-META-OBJECT() {
        Perl6::Metamodel::EnumHOW.new_type(
            :name($!name.canonicalize),
            :base_type($!base-type)
        )
    }
}

class RakuAST::Type::Subset
  is RakuAST::Type
  is RakuAST::Lookup
  is RakuAST::Declaration
  is RakuAST::BeginTime
  is RakuAST::TraitTarget
  is RakuAST::StubbyMeta
  is RakuAST::Attaching
  is RakuAST::PackageInstaller
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Name       $.name;
    has RakuAST::Trait::Of  $.of;
    has RakuAST::Expression $.where;

    has Mu $!current-package;
    has Mu $!block;

    method new(          str :$scope,
               RakuAST::Name :$name!,
          RakuAST::Trait::Of :$of,
         RakuAST::Expression :$where,
                        List :$traits,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Type::Subset, '$!of', $of) if $of;
        if $where {
            # The $!where attribute is for deparsing, the $!block attribute
            # is for the actual action.
            nqp::bindattr($obj, RakuAST::Type::Subset, '$!where', $where);
            nqp::bindattr($obj, RakuAST::Type::Subset, '$!block', $where);
        }
        $obj.set-traits($traits) if $traits;
        $obj.set-WHY($WHY);
        $obj.set-resolution($obj);
        $obj
    }

    method set-traits($traits) {
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

    method dba() { 'subset' }

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
        $visitor($!block) if $!block;
        # External constants break if visited with missing IMPL-QAST-DECL.
        # Adding a sensible IMPL-QAST-DECL results in lexical declarations
        # for things like Int, which will break if added more than once.
        $visitor($!of)
          if $!of
          && !nqp::istype($!of, RakuAST::Declaration::External::Constant);
        $visitor(self.WHY) if self.WHY;
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
            :value(nqp::eqaddr($type-object, Mu) ?? self.stubbed-meta-object !! $type-object),
            :scope(self.scope);
    }

    method is-begin-performed-after-children() { True }

    method PERFORM-BEGIN-AFTER-CHILDREN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self);

        my $block := $!block;
        if $block
          && !$block.IMPL-CURRIED
          && (!nqp::istype($block, RakuAST::Code)
               || nqp::istype($block, RakuAST::RegexThunk
             )
        ) {
            $block := RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyPostfix.new(
                                operand => RakuAST::ApplyPostfix.new(
                                    operand => $!block,
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
            nqp::bindattr(self, RakuAST::Type::Subset, '$!block', $block);
            $block.IMPL-CHECK($resolver, $context, False);
        }

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
        # Update the Stash's name, too.
        nqp::bindattr_s($type.WHO, Stash, '$!longname', $type.HOW.name($type));

        self.IMPL-INSTALL-PACKAGE(
          $resolver, self.scope, $!name, $package, :meta-object(Mu)
        );

        self.meta-object; # Finish meta-object setup so compile time type-checks will be correct
        if $block && $block.IMPL-CURRIED {
            # Cache QAST with expression as the BEGIN time stub wont know how to get that
            $block.IMPL-CURRIED.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>, :expression($block));
        }
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

        $type.HOW.set_of($type, $!of.type.meta-object)
          if $!of;
        $type.HOW.set_where($type, $block
          ?? $block.IMPL-CURRIED
            ?? $block.IMPL-CURRIED.meta-object
            !! $block.compile-time-value
          !! Mu
        );

        $type
    }
}
