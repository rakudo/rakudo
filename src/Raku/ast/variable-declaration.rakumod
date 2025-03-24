# All initializers do this marker. An initializer is the `= 42` part in a
# declaration like `my $a = 42`.
class RakuAST::Initializer
  is RakuAST::Node
{
    has RakuAST::ExpressionThunk $!thunk;

    method is-binding() { False }

    method IMPL-COMPILE-TIME-VALUE(RakuAST::Resolver $resolver,
        RakuAST::IMPL::QASTContext $context, Mu :$invocant-compiler)
    {
        RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE(self.expression, $resolver, $context);
    }

    method IMPL-THUNK-EXPRESSION(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $expression := self.expression;
        my $thunk := Nil;
        unless (nqp::istype($expression, RakuAST::Code)) {
            $thunk := $expression.outer-most-thunk;
            unless ($thunk) {
                $thunk := RakuAST::ExpressionThunk.new;
                $expression.wrap-with-thunk($thunk);
                $thunk.IMPL-STUB-CODE($resolver, $context);
            }
            nqp::bindattr(self, RakuAST::Initializer, '$!thunk', $thunk);
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $expression := self.expression;
        nqp::istype($expression, RakuAST::Code)
            ?? $expression.IMPL-TO-QAST($context)
            !! $!thunk.IMPL-QAST-BLOCK($context, :expression($expression))
    }
}

# Role for classes that initialize from an expression
class RakuAST::Initializer::Expression
  is RakuAST::Initializer
{
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression) {
        my $obj := nqp::create(self);
        $obj.set-expression($expression);
        $obj
    }

    method set-expression(RakuAST::Expression $expression) {
        nqp::bindattr(self, RakuAST::Initializer::Expression, '$!expression',
          $expression // RakuAST::Expression);
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }

    method IMPL-TO-QAST(
      RakuAST::IMPL::QASTContext $context, Mu :$invocant-qast
    ) {
        $!expression.IMPL-TO-QAST($context)
    }
}

# An assignment (`=`) initializer.
class RakuAST::Initializer::Assign
  is RakuAST::Initializer::Expression
{
    method is-binding() { False }
}

# A bind (`:=`) initializer.
class RakuAST::Initializer::Bind
  is RakuAST::Initializer::Expression
{
    method is-binding() { True }
}

# An mutating method call (`.=`) initializer.
class RakuAST::Initializer::CallAssign
  is RakuAST::Initializer
{
    has RakuAST::Postfixish $.postfixish;

    method new(RakuAST::Postfixish $postfixish) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Initializer::CallAssign, '$!postfixish', $postfixish);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!postfixish);
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!postfixish.add-colonpair($pair);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, Mu :$invocant-qast) {
        $!postfixish.IMPL-POSTFIX-QAST($context, $invocant-qast)
    }

    method IMPL-COMPILE-TIME-VALUE(RakuAST::Resolver $resolver,
        RakuAST::IMPL::QASTContext $context, Mu :$invocant-compiler)
    {
        self.postfixish.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new, $invocant-compiler)
    }

    method IMPL-THUNK-EXPRESSION(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<null>)
    }
}

# Consuming class has to implement IMPL-SIGIL-TYPE which returns the resolution
# for the lookup created by IMPL-SIGIL-LOOKUP.
class RakuAST::ContainerCreator {
    has Mu $!explicit-container-base-type;
    has Mu $!conflicting-base-type;
    has Bool $.forced-dynamic;
    has Bool $!initialized;
    has Mu $.container-base-type;
    has Mu $.container-type;
    has ContainerDescriptor $.container-descriptor;
    has Mu $.bind-constraint;

    method IMPL-SET-EXPLICIT-CONTAINER-BASE-TYPE(Mu $type) {
        if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
            nqp::bindattr(self, RakuAST::ContainerCreator, '$!conflicting-base-type', $!explicit-container-base-type);
        }
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!explicit-container-base-type', $type);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!initialized', False);
    }

    method IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE() {
        !nqp::eqaddr($!explicit-container-base-type, Mu)
    }

    method IMPL-HAS-CONFLICTING-BASE-TYPE() {
        !nqp::eqaddr($!conflicting-base-type, Mu)
    }

    method IMPL-EXPLICIT-CONTAINER-BASE-TYPE() {
        $!explicit-container-base-type
    }

    method IMPL-CONFLICTING-BASE-TYPE() {
        $!conflicting-base-type
    }

    method IMPL-SIGIL-LOOKUP() {
        my $types := nqp::hash(
            '@', 'Positional',
            '%', 'Associative',
            '&', 'Callable',
        );
        my $sigil := self.sigil;

        nqp::existskey($types, $sigil)
            ?? RakuAST::Type::Setting.new(RakuAST::Name.from-identifier($types{$sigil}))
            !! nqp::null
    }

    method IMPL-CALCULATE-TYPES(Mu $of, Mu :$key-type) {
        return Nil if $!initialized;

        # Form the container type.
        my str $sigil := self.sigil;
        my $container-base-type;
        my $container-type;
        my $default := Any;
        my $bind-constraint := Mu;
        if nqp::eqaddr($!explicit-container-base-type, Mu) {
            if $sigil eq '@' {
                $bind-constraint := self.IMPL-SIGIL-TYPE;
                $container-base-type := nqp::objprimspec($of) ?? array !! Array;
                if self.type {
                    {
                        $container-type := $container-base-type.HOW.parameterize($container-base-type, $of);
                        CATCH {
                            if $*COMPILING_CORE_SETTING == 1 {
                                $container-type := $container-base-type
                            }
                            else {
                                nqp::die($_);
                            }
                        }
                    }
                    $bind-constraint := $bind-constraint.HOW.parameterize($bind-constraint, $of);
                }
                else {
                    $container-type := Array;
                }
            }
            elsif $sigil eq '%' {
                $container-base-type := Hash;
                $bind-constraint := self.IMPL-SIGIL-TYPE;
                if $key-type =:= NQPMu {
                    if self.type {
                        $container-type := Hash.HOW.parameterize(Hash, $of);
                        $bind-constraint := $bind-constraint.HOW.parameterize($bind-constraint, $of);
                    }
                    else {
                        $container-type := Hash;
                    }
                }
                else {
                    if self.type {
                        $container-type := Hash.HOW.parameterize(Hash, $of, $key-type);
                        $bind-constraint := $bind-constraint.HOW.parameterize(
                            $bind-constraint, $of, $key-type);
                    }
                    else {
                        $container-type := Hash.HOW.parameterize(
                            Hash, $!explicit-container-base-type, $key-type);
                        $bind-constraint := $bind-constraint.HOW.parameterize(
                            $bind-constraint, $!explicit-container-base-type, $key-type);
                    }
                }
            }
            elsif $sigil eq '&' {
                $container-base-type := Scalar;
                my $Callable := self.IMPL-SIGIL-TYPE;
                $container-type := self.type
                    ?? $Callable.HOW.parameterize($Callable, $of)
                    !! $Callable;
                $default := $Callable;
                $bind-constraint := $container-type;
            }
            else {
                $container-base-type := Scalar;
                $container-type := $of; #FIXME pretty sure container-type should also be Scalar
                $bind-constraint := $of;
            }
        }
        else {
            $container-type := self.type
                ?? $!explicit-container-base-type.HOW.parameterize($!explicit-container-base-type, $of)
                !! $!explicit-container-base-type;
        }

        nqp::bindattr(self, RakuAST::ContainerCreator, '$!initialized', True);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-base-type', $container-base-type);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-type', $container-type);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!bind-constraint', $bind-constraint);

        # Form container descriptor.

        $default := RakuAST::Type.IMPL-MAYBE-NOMINALIZE($of) if self.type;
        my int $dynamic := self.twigil eq '*' ?? 1 !! self.forced-dynamic ?? 1 !! 0;
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-descriptor', (
                nqp::eqaddr($of, Mu)
                ?? ContainerDescriptor::Untyped
                !! ContainerDescriptor
            ).new(:$of, :$default, :$dynamic, :name(self.lexical-name))
        );

        Nil
    }

    method IMPL-CONTAINER-DESCRIPTOR(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of);

        self.container-descriptor
    }

    method IMPL-CONTAINER-TYPE(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of);

        self.container-type
    }

    method IMPL-BIND-CONSTRAINT(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of);

        self.bind-constraint
    }

    method IMPL-CONTAINER(Mu $of, Mu $cont-desc, Bool :$attribute) {
        self.IMPL-CALCULATE-TYPES($of);

        my $container-type := self.container-type;
        # Form the container.
        my str $sigil := self.sigil;
        if nqp::eqaddr($!explicit-container-base-type, Mu) {
            if $sigil ne '@' && $sigil ne '%' {
                if nqp::objprimspec($of) {
                    nqp::die("Natively typed state variables not yet implemented") if self.scope eq 'state';
                    return nqp::null unless $attribute;
                }

                $container-type := Scalar;
            }
            my $container := nqp::create($container-type);
            try nqp::bindattr($container, $!container-base-type, '$!descriptor', $cont-desc);
            unless $sigil eq '@' || $sigil eq '%' {
                nqp::bindattr($container, $!container-base-type, '$!value', $cont-desc.default);
            }
            $container
        }
        else {
            $!explicit-container-base-type
        }
    }
}

class RakuAST::TraitTarget::Variable
  is RakuAST::TraitTarget
  is RakuAST::Meta
  is RakuAST::ImplicitLookups
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has str $!name;
    has str $!scope;
    has Mu $!cont;
    has Mu $!code-object;
    has Mu $!slash;

    method new(str $name, str $scope, Mu $cont, Mu $code-object, Mu $slash) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::TraitTarget::Variable, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::TraitTarget::Variable, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::TraitTarget::Variable, '$!cont', $cont);
        nqp::bindattr($obj, RakuAST::TraitTarget::Variable, '$!code-object', $code-object);
        nqp::bindattr($obj, RakuAST::TraitTarget::Variable, '$!slash', $slash);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Variable')),
        ])
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;
    }

    method PRODUCE-META-OBJECT() {
        my $Variable  := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value;
        my $varvar := nqp::create($Variable);
        nqp::bindattr_s($varvar, $Variable, '$!name', $!name);
        nqp::bindattr_s($varvar, $Variable, '$!scope', $!scope);
        nqp::bindattr($varvar, $Variable, '$!var', $!cont);
        nqp::bindattr($varvar, $Variable, '$!block', $!code-object);
        nqp::bindattr($varvar, $Variable, '$!slash', $!slash);
        nqp::assign(
            nqp::getattr($varvar, $Variable, '$!implicit-lexical-usage'),
            True);
        $varvar
    }
}

# Base class for variable declarations
class RakuAST::VarDeclaration
  is RakuAST::Declaration { }

# A basic constant declaration of the form `my Type constant $foo = 42`
class RakuAST::VarDeclaration::Constant
  is RakuAST::VarDeclaration
  is RakuAST::TraitTarget
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::CompileTimeValue
  is RakuAST::ImplicitLookups
  is RakuAST::Term
{
    has str                      $.name;
    has RakuAST::Initializer     $.initializer;
    has RakuAST::Type            $.type;
    has Mu                       $!value;
    has Mu                       $!package;

    method new(
      str           :$scope,
      RakuAST::Type :$type,
      str           :$name!,
      RakuAST::Initializer :$initializer!,
      List          :$traits
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Constant,'$!name',$name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Constant, '$!initializer',
            $initializer // RakuAST::Initializer);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Constant, '$!type',
          $type // RakuAST::Type);
        $obj.set-traits($traits) if $traits;
        $obj
    }

    method lexical-name()   { $!name }
    method default-scope()  { 'our'   }
    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our']) }
    method is-simple-lexical-declaration() { True }
    method needs-sink-call() { False }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            self.sigil eq '@'
                ?? RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Positional'))
                !! self.sigil eq '%'
                    ?? RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Associative'))
                    !! self.sigil eq '&'
                        ?? RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Callable'))
                        !! $!type
                            ?? $!type
                            !! nqp::null,

        ])
    }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
        $visitor($!initializer) if $!initializer;
        self.visit-traits($visitor);
    }

    method sigil {
        nqp::substr($!name, 0, 1)
    }

    method PERFORM-BEGIN(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $value := $!initializer.IMPL-COMPILE-TIME-VALUE(
            $resolver, $context, :invocant-compiler(-> { $!type ?? $!type.IMPL-VALUE-TYPE.meta-object !! Mu }));
        $value := $value.Map
            if nqp::eqat($!name,'%',0)
            && !nqp::istype($value, self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].meta-object);
        $value := $value.cache
            if nqp::eqat($!name, '@', 0)
            && !nqp::istype($value, self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].meta-object);
        $!initializer.IMPL-THUNK-EXPRESSION($resolver, $context);
        nqp::bindattr(self, RakuAST::VarDeclaration::Constant, '$!value', $value);

        if self.scope eq 'our' {
            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(
              self, RakuAST::VarDeclaration::Constant, '$!package',
              $resolver.current-package
            );
            my $name := nqp::getattr_s(self, RakuAST::VarDeclaration::Constant, '$!name');
            my $stash := $resolver.IMPL-STASH-HASH($!package);
            if nqp::existskey($stash, $name) {
                nqp::die("already have an 'our constant $name' in the package");
            }
            nqp::bindkey($stash, $name, $!value);
        }

        self.apply-traits(
          $resolver, $context, self, :SYMBOL(RakuAST::StrLiteral.new($!name))
        );
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        self.add-trait-sorries;

        my $sigil := self.sigil;
        if $!type && ($sigil eq '@' || $sigil eq '%' || $sigil eq '&') {
            self.add-sorry: $resolver.build-exception('X::ParametricConstant');
        }

        my $type := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
        if $type && !nqp::istype($!initializer, RakuAST::Initializer::CallAssign) && !nqp::objprimspec($type.meta-object) {
            unless nqp::istype($!value, $type.meta-object) {
                my $name := nqp::getattr_s(self, RakuAST::VarDeclaration::Constant, '$!name');
                self.add-sorry:
                  $resolver.build-exception: 'X::Comp::TypeCheck',
                    operation => 'constant declaration of ' ~ ($name || '<anon>'),
                    expected  => $type.meta-object,
                    got       => $!value;
                return False;
            }
        }
        True
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $constant := QAST::Stmts.new(
            $!initializer.IMPL-QAST-DECL($context),
            QAST::Var.new(
              :decl('static'), :scope('lexical'), :name($!name), :value($!value)
            )
        );
        if self.scope eq 'our' {
            $context.ensure-sc($!package);
            QAST::Op.new(
                :op('bindkey'),
                QAST::Op.new(:op('who'), QAST::WVal.new(:value($!package))),
                QAST::SVal.new(:value($!name)),
                $constant
            )
        }
        else {
            $constant
        }
    }

    method compile-time-value() { $!value }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(:name($!name), :scope<lexical>)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(:name($!name), :scope<lexical>);
    }
}

class RakuAST::Expression::QAST
  is RakuAST::Expression
{
    has QAST::Node $!qast;
    method new(QAST::Node $qast) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Expression::QAST, '$!qast', $qast);
        $obj
    }
    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!qast;
    }
}

# A basic variable declaration of the form `my SomeType $foo = 42` or
# `has Foo $x .= new`.
class RakuAST::VarDeclaration::Simple
  is RakuAST::VarDeclaration
  is RakuAST::ImplicitLookups
  is RakuAST::TraitTarget
  is RakuAST::ContainerCreator
  is RakuAST::Meta
  is RakuAST::ParseTime
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Term
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Type        $.type;
    has RakuAST::Name        $.desigilname;
    has str                  $.sigil;
    has str                  $.twigil;
    has str                  $!storage-name;
    has RakuAST::Initializer $.initializer;
    has RakuAST::Method      $.initializer-method;
    has RakuAST::SemiList    $.shape;
    has RakuAST::Package     $.attribute-package;
    has RakuAST::Role        $!generics-package;
    has RakuAST::Method      $!accessor;
    has RakuAST::Type        $!conflicting-type;
    has RakuAST::Expression  $.where;
    has RakuAST::Type        $.original-type;
    has Bool                 $!is-parameter;
    has Bool                 $!is-rw;
    has Bool                 $!is-ro;
    has Bool                 $!is-bindable;
    has Bool                 $!already-declared;
    has RakuAST::Code        $!block;

    has Mu $!container-initializer;
    has Mu $!package;

    method new(          str :$scope,
               RakuAST::Name :$desigilname!,
                         str :$sigil,
                         str :$twigil,
               RakuAST::Type :$type,
                        List :$traits,
        RakuAST::Initializer :$initializer,
           RakuAST::SemiList :$shape,
                        Bool :$forced-dynamic,
                        Bool :$is-parameter,
         RakuAST::Expression :$where,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        if $desigilname.is-empty {
            nqp::die('Cannot use RakuAST::VarDeclaration::Simple to declare an anonymous variable; use RakuAST::VarDeclaration::Anonymous');
        }

        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!desigilname', $desigilname);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!sigil', $sigil);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!twigil', $twigil || '');

        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!type',
          $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!shape',
          $shape // RakuAST::SemiList);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!initializer',
            $initializer // RakuAST::Initializer);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!accessor',
          RakuAST::Method);
        nqp::bindattr($obj, RakuAST::ContainerCreator, '$!forced-dynamic',
          $forced-dynamic ?? True !! False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-parameter',
          $is-parameter ?? True !! False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!where',
          $where // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!original-type',
          $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-rw', False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-ro', False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-bindable', True);

        if $WHY {
            $scope && $obj.is-attribute
              ?? $obj.set-WHY($WHY)
              !! nqp::die("Declarator doc only supported on scope 'has'");
        }

        $obj
    }

    method set-initializer(RakuAST::Initializer $initializer) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!initializer',
            $initializer // RakuAST::Initializer);
    }

    method name() {
        self.sigil ~ (self.twigil // (self.is-attribute ?? '!' !! '')) ~ $!desigilname.canonicalize;
    }

    method lexical-name() {
        self.twigil eq '.' ?? self.sigil ~ '!' ~ self.desigilname.canonicalize !! self.name
    }

    method set-type(RakuAST::Type $type) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!type', $type);
    }

    method set-rw() {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!is-rw', True);
    }

    method set-ro(Bool $ro) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!is-ro', $ro);
    }

    method set-bindable(Bool $bindable) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!is-bindable', $bindable);
    }

    method set-already-declared() {
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!already-declared', True);
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        nqp::die("Cannot add colonpair to variable declaration without initializer")
            unless $!initializer;
        $!initializer.add-colonpair($pair);
    }

    method report-redeclaration() {
        !$!already-declared
    }

    # Generate a lookup of this variable, already resolved to this declaration.
    method generate-lookup() {
        if self.is-lexical {
            my $lookup := RakuAST::Var::Lexical.new(self.name);
            $lookup.set-resolution(self);
            $lookup
        }
        else {
            nqp::die('Cannot generate lookup of simple var for scope ' ~ self.scope);
        }
    }

    method can-be-bound-to() {
        # Must be lexical and non-native.
        if $!is-bindable && (self.scope eq 'my' || self.scope eq 'our' || self.scope eq 'state') {
            my str $sigil := self.sigil;
            return True if $sigil eq '@' || $sigil eq '%';
            return True unless $!type;
            return True unless nqp::objprimspec(
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value
            );
        }
        False
    }

    method visit-children(Code $visitor) {
        $visitor($!type)        if nqp::isconcrete($!type);
        $visitor($!initializer) if nqp::isconcrete($!initializer);
        $visitor($!initializer-method) if nqp::isconcrete($!initializer-method);
        $visitor($!shape)       if nqp::isconcrete($!shape);
        $visitor($!accessor)    if nqp::isconcrete($!accessor);
        $visitor($!where)       if nqp::isconcrete($!where);
        $visitor($!original-type) if nqp::isconcrete($!original-type) && !($!original-type =:= $!type);
        self.visit-traits($visitor);
        $visitor(self.WHY) if self.WHY;
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

    method is-attribute() {
        my str $scope := self.scope;
        $scope eq 'has' || $scope eq 'HAS'
    }

    method IMPL-OF-TYPE() {
        $!type
            ?? (nqp::istype($!type, RakuAST::Lookup)
                ?? ($!type.is-resolved
                    ?? $!type
                    !! self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0]
                ).resolution
                !! $!type
             ).compile-time-value
          !! Mu;
    }

    method return-type() {
        self.IMPL-BIND-CONSTRAINT(self.IMPL-OF-TYPE)
    }

    method IMPL-CALCULATE-TYPES(Mu $of) {
        my &calculate-types := nqp::findmethod(RakuAST::ContainerCreator, 'IMPL-CALCULATE-TYPES');
        $!shape && self.sigil eq '%'
            ?? &calculate-types(self, $of, :key-type($!shape.code-statements[0].expression.compile-time-value))
            !! &calculate-types(self, $of);
    }

    # Runs before we parse the initializer, so we can setup a proper environment for resolving
    # symbols used by the initializer.
    method PERFORM-PARSE(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $block := $resolver.find-attach-target('block');
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!block', $block);

        if self.is-attribute {
            my $method := RakuAST::Method.new(
              :signature(RakuAST::Signature.new(
                :parameters([
                  RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new(:name<$_>)
                  )
                ])
              )),
              :body(RakuAST::Blockoid.new(
                RakuAST::StatementList.new().to-parse-time($resolver, $context)
              ))
            ).to-parse-time($resolver, $context);
            nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!initializer-method', $method);
        }
    }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        if self.is-attribute || self.twigil eq '.' {
            my $attribute-package := $resolver.find-attach-target('package');
            if $attribute-package {
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!attribute-package',
                    $attribute-package);
                if self.is-attribute && !$!attribute-package.can-have-attributes {
                    $resolver.add-worry:  # XXX should be self.add-worry
                        $resolver.build-exception: 'X::Attribute::Package',
                            name         => self.name,
                            package-kind => $!attribute-package.declarator;
                }
            }
            else {
                # TODO check-time error
                my $package := nqp::getlexdyn('$?PACKAGE');
                # In class Foo { EVAL q[has $.foo] } we won't find a package attach target
                # because compile time is long over, yet there will be a $?PACKAGE. Throwing
                # a NoPackage here would be confusing. It would be better to throw an error
                # explaining that the package is already composed. Alas there's a spec test
                # that requires this to be silently ignored, so that's what we do for now.
                if self.is-attribute && !nqp::can($package.HOW, 'add_attribute') {
                    $resolver.build-exception('X::Attribute::NoPackage', name => self.name).throw;
                }
            }
        }
        elsif self.scope eq 'our' || $!desigilname.is-multi-part {
            my $package := $resolver.current-package;
            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!package',
                $package);
        }
        nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!generics-package',
            $resolver.find-attach-target('generics-pad'));

        # Process traits for `is Type` and `of Type`, which get special
        # handling by the compiler.
        my @late-traits;
        my @traits := self.IMPL-UNWRAP-LIST(self.traits);

        my $of-type;
        for @traits {
            if nqp::istype($_, RakuAST::Trait::Of) {
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!conflicting-type', $!type) if $!type;
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!type', $of-type := $_.type);
                next;
            }
            elsif nqp::istype($_, RakuAST::Trait::Is) {
                my $type := $_.type;

                # an actual type
                if nqp::isconcrete($type) && !$_.argument
                    && (!nqp::istype($type, RakuAST::Lookup) || $type.is-resolved)
                {
                    self.IMPL-SET-EXPLICIT-CONTAINER-BASE-TYPE($type.meta-object);
                    next;
                }
            }
            nqp::push(@late-traits, $_);
        }

        my $subset;
        if my $where := $!where {
            my $type := $of-type // self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
            my $type-name := $type ?? $type.name.canonicalize !! "Mu";
            my $subset-name := RakuAST::Name.from-identifier: QAST::Node.unique($type-name ~ '+anon_subset');
            $subset := RakuAST::Type::Subset.new: :name($subset-name), :of($type ?? RakuAST::Trait::Of.new($type) !! Mu), :$where;
            $subset.ensure-begin-performed($resolver, $context);
            self.set-type($subset);
        }

        # Apply any traits.
        self.set-traits(self.IMPL-WRAP-LIST(@late-traits));

        if self.is-attribute {
            if ($!sigil eq '@' && $!shape) || self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE || $subset {
                my $args := $!shape
                    ?? RakuAST::ArgList.new(
                        RakuAST::ColonPair::Value.new(:key<shape>, :value($!shape))
                    )
                    !! RakuAST::ArgList.new;
                my $of := $subset ?? $subset.meta-object !! self.IMPL-OF-TYPE;

                my $container-initializer-ast := RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Declaration::ResolvedConstant.new(
                        :compile-time-value(
                            $!sigil eq '$'
                                ?? self.meta-object
                                !! self.IMPL-CONTAINER-TYPE($of)
                        )
                    ),
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('new'),
                        :$args
                    )
                ).to-begin-time($resolver, $context);

                my $thunk := RakuAST::BlockThunk.new(:expression($container-initializer-ast));
                $container-initializer-ast.wrap-with-thunk($thunk);
                $thunk.to-begin-time($resolver, $context);
                $resolver.current-scope.add-generated-lexical-declaration(
                    RakuAST::VarDeclaration::Implicit::Block.new(
                        :block($thunk)
                    ).to-begin-time($resolver, $context)
                );
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!container-initializer',
                    $thunk.meta-object);
            }

            if $!initializer {
                my $initializer := $!initializer;
                if nqp::istype($initializer, RakuAST::Initializer::Assign) && $initializer.expression.has-compile-time-value && !nqp::istype($initializer.expression, RakuAST::Code) {
                    self.add-trait(
                        RakuAST::Trait::WillBuild.new($initializer.expression).to-begin-time($resolver, $context)
                    );
                }
                else {
                    my $method := $!initializer-method;
                    $method.body.statement-list.add-statement(
                        RakuAST::Statement::Expression.new(
                            :expression(
                                nqp::istype($initializer, RakuAST::Initializer::CallAssign)
                                ?? RakuAST::ApplyPostfix.new(
                                    operand => $!type.IMPL-VALUE-TYPE,
                                    postfix => $initializer.postfixish
                                )
                                !! RakuAST::Call::Name.new(
                                    :name(RakuAST::Name.from-identifier('return')),
                                    :args(RakuAST::ArgList.new($initializer.expression))
                                )
                            )
                        )
                    );
                    $method.to-begin-time($resolver, $context);
                    $!attribute-package.add-generated-lexical-declaration($method);
                    self.add-trait(
                        RakuAST::Trait::WillBuild.new($method).to-begin-time($resolver, $context)
                    );
                }
            }
            else {
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!initializer-method', RakuAST::Method);
            }

            # For attributes our meta-object is an appropriate Attribute instance
            self.apply-traits($resolver, $context, self);

            my $meta-object := self.meta-object;
            if self.scope eq 'HAS' && $meta-object.type.REPR eq 'CArray' && $!shape {
                my @dimensions := nqp::list_i();
                my $shape := self.IMPL-BEGIN-TIME-EVALUATE($!shape, $resolver, $context);
                my $elems := nqp::unbox_i(self.IMPL-BEGIN-TIME-EVALUATE($!shape, $resolver, $context));
                nqp::push_i(@dimensions, $elems);
                nqp::bindattr($meta-object, $meta-object.WHAT, '$!dimensions', @dimensions);
            }

            $!attribute-package.ATTACH-ATTRIBUTE(self) if $!attribute-package && $!attribute-package.can-have-attributes;
        }
        else {
            # For other variables the meta-object is just the container, but we
            # need instances of Variable
            my $meta := self.meta-object;
            my $target := RakuAST::TraitTarget::Variable.new(
                self.name,
                nqp::getattr(self, RakuAST::Declaration, '$!scope'),
                $meta,
                $!block.stubbed-meta-object,
                Mu
            );
            # Get around RakuAST compiler deconting all arguments:
            nqp::bindattr($target, RakuAST::TraitTarget::Variable, '$!cont', $meta);
            $target.to-begin-time($resolver, $context); # TODO maybe also check?

            if self.twigil eq '.' {
                my $variable-access := RakuAST::Var::Lexical.new(self.name);
                $variable-access.set-resolution(self);
                my $accessor := RakuAST::Method::ClassAccessor.new(
                    :scope<has>,
                    :name(RakuAST::Name.from-identifier(self.desigilname.canonicalize)),
                    :body(RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(:expression($variable-access)),
                        ),
                    )),
                );
                nqp::bindattr(self, RakuAST::VarDeclaration::Simple, '$!accessor', $accessor);
                $resolver.push-scope($accessor);
                $accessor.to-begin-time($resolver, $context); # TODO maybe also check?
                $resolver.pop-scope();
            }

            self.apply-traits($resolver, $context, $target);

            my $of := $subset ?? $subset.meta-object !! self.IMPL-OF-TYPE;
            if self.scope eq 'has' && $of.HOW.archetypes.generic {
                $!generics-package.IMPL-ADD-GENERIC-LEXICAL(self);
            }
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        self.add-sorry(
          $resolver.build-exception: 'X::Adhoc',
            :message('Cannot declare an anonymous variable with a twigil')
        ) if self.twigil && self.desigilname.is-empty;

        self.add-sorry(
          $resolver.build-exception: 'X::Dynamic::Package',
            :symbol(self.name)
        ) if self.twigil eq '*' && self.desigilname.is-multi-part;

        self.add-sorry(
          $resolver.build-exception: 'X::Syntax::Variable::Twigil',
            :twigil(self.twigil), :scope(self.scope), :name(self.name)
        ) if self.twigil eq '!' && (self.scope eq 'my' || self.scope eq 'our' || self.scope eq 'state');

        self.add-sorry(
          $resolver.build-exception: 'X::Syntax::Variable::ConflictingTypes',
            :outer($!conflicting-type.compile-time-value), :inner($!type.compile-time-value)
        ) if $!conflicting-type;

        self.add-sorry(
          $resolver.build-exception: 'X::Syntax::Variable::ConflictingTypes',
            outer => self.IMPL-CONFLICTING-BASE-TYPE,
            inner => self.IMPL-EXPLICIT-CONTAINER-BASE-TYPE,
        ) if self.IMPL-HAS-CONFLICTING-BASE-TYPE;

        if (self.initializer) {
            my @found := self.IMPL-UNWRAP-LIST(self.find-nodes-exclusive(
                RakuAST::Var::Lexical,
                :condition(-> $node {
                    $node.is-resolved && $node.resolution =:= self
                }),
                :stopper(-> $node {
                    nqp::istype($node, RakuAST::Block) || nqp::istype($node, RakuAST::Routine)
                })));
            if @found {
                self.add-sorry(
                    $resolver.build-exception: 'X::Syntax::Variable::Initializer', :name(self.name)
                );
            }
        }

        my $type := self.type;
        # Subset type checking can have side effects, so don't do that at compile time.
        if nqp::istype($type, RakuAST::Type::Simple) && !nqp::istype($type.meta-object.HOW, Perl6::Metamodel::SubsetHOW) {
            my $initializer := self.initializer;
            if nqp::istype($initializer,RakuAST::Initializer::Assign)
                 || nqp::istype($initializer,RakuAST::Initializer::Bind) {

                my $of := self.IMPL-OF-TYPE;
                if self.sigil eq '$' && (my int $prim-spec := nqp::objprimspec($of)) && $initializer.is-binding {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Bind::NativeType',
                            :name(self.name);
                }

                my $expression := $initializer.expression;
                if nqp::istype($expression, RakuAST::Literal) {
                    my $vartype := $type.meta-object;
                    if nqp::objprimspec($vartype) {
                        $vartype := $vartype.HOW.mro($vartype)[1];
                    }

                    my $value := $expression.compile-time-value;
                    if !nqp::istype($value, $vartype)
                      && nqp::istype(
                           $vartype,
                           $resolver.type-from-setting('Numeric')
                         ) {
                        self.add-sorry:
                          $resolver.build-exception: 'X::Syntax::Number::LiteralType',
                            :varname(self.name), :$vartype, :$value;
                    }
                }
            }
        }

        if self.sigil eq '$' && !self.initializer && !$!is-parameter && (!self.is-attribute || !self.meta-object.required) {
            my $descriptor := self.container-descriptor;
            my $ddefault := $descriptor.default;
            my $bind-constraint := self.bind-constraint;
            unless $ddefault.HOW.archetypes.generic || $bind-constraint.HOW.archetypes.generic {
                my $matches;
                my $maybe := 0;
                try {
                    $matches := nqp::istype($ddefault, $bind-constraint);
                }
                unless $matches {
                    self.add-sorry(
                        $resolver.build-exception: 'X::Syntax::Variable::MissingInitializer',
                            what => self.scope eq 'has' || self.scope eq 'HAS' ?? 'attribute' !! 'variable',
                            type => nqp::how($bind-constraint).name($bind-constraint),
                            implicit => nqp::istype($type, RakuAST::Type::Definedness)
                                ?? $type.IMPL-IMPLICIT
                                !! '',
                    );
                }
            }
        }

        if self.twigil eq '.' && !self.is-attribute && !$!attribute-package {
            my $package := nqp::getlexdyn('$?PACKAGE');
            $resolver.add-worry:
                $resolver.build-exception('X::AdHoc', payload => "Useless generation of accessor method in " ~
                (nqp::istype($package.HOW, Perl6::Metamodel::AttributeContainer)
                    ?? $package.HOW.name($package)
                    !! "mainline"));
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        my str $scope := self.scope;
        # If we have a type, we need to resolve that.  Otherwise assume Mu
        @lookups.push($!type || nqp::null);

        # If we're has/HAS scope, we need Nil to evaluate to.
        @lookups.push($scope eq 'has' || $scope eq 'HAS'
          ?? RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil'))
          !! nqp::null
        );

        @lookups.push(self.IMPL-SIGIL-LOOKUP);

        @lookups.push(
            RakuAST::Var::Lexical.new(
                :sigil('$'),
                :twigil<?>,
                :desigilname(RakuAST::Name.from-identifier('CLASS')),
            )
        );

        self.IMPL-WRAP-LIST(@lookups)
    }

    method IMPL-SIGIL-TYPE() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[2].resolution.compile-time-value
    }

    method PRODUCE-META-OBJECT() {
        # If it's our-scoped, then container is vivified via. package access.
        my str $scope := self.scope;

        # Calculate the type.
        my $of := self.IMPL-OF-TYPE;
        my $default := self.sigil eq '&'
            ?? $!type
                ?? self.IMPL-SIGIL-TYPE.HOW.parameterize(self.IMPL-SIGIL-TYPE, $of)
                !! self.IMPL-SIGIL-TYPE
            !! $of;
        my $descriptor := self.IMPL-CONTAINER-DESCRIPTOR($default);

        # `my %h{Any}` creates this shape declaration:
        #
        # shape => RakuAST::SemiList.new(
        #   RakuAST::Statement::Expression.new(
        #     expression => RakuAST::Type::Simple.new(   <-- shape type
        #       RakuAST::Name.from-identifier("Any")
        #     )
        #   )
        # )
        my $type        := self.IMPL-BIND-CONSTRAINT($of);

        # If it's has scoped, we'll need to build an attribute.
        if $scope eq 'has' || $scope eq 'HAS' {
            return Nil unless $!attribute-package && $!attribute-package.can-have-attributes; # See explanation in PERFORM-BEGIN

            my $meta-object := $!attribute-package.attribute-type.new(
              name => self.sigil ~ '!' ~ self.desigilname.canonicalize,
              type => $scope eq 'HAS' ?? self.IMPL-CONTAINER-TYPE($of) !! $type,
              has_accessor          => self.twigil eq '.',
              container_descriptor  => $descriptor,
              auto_viv_container    => self.IMPL-CONTAINER($of, $descriptor, :attribute),
              # For classes package would be just $!attribute-package.compile-time-value
              # but for roles we have to use the $?CLASS generic to defer instantiation
              # to consuming class' compose times.
              package               => self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[3].resolution.maybe-compile-time-value,
              container_initializer => $!container-initializer,
            );
            nqp::bindattr_i($meta-object,$meta-object.WHAT,'$!inlined',1)
              if $scope eq 'HAS';
            $meta-object
        }

        # An "our" that is already installed
        elsif $scope eq 'our' && $!package.WHO.EXISTS-KEY(self.name) {
            $!package.WHO.AT-KEY(self.name)
        }

        # Otherwise, it's lexically scoped, so the meta-object is just the
        # container, if any.
        else {
            my $container := self.IMPL-CONTAINER($of, $descriptor);
            $!package.WHO.BIND-KEY(self.name, $container) if $scope eq 'our';
            $container
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my str $scope := self.scope;
        my $of := $!where ?? $!type.meta-object !! self.IMPL-OF-TYPE;

        return QAST::Op.new(:op<null>) if $!already-declared;

        if $scope eq 'my' && !$!desigilname.is-multi-part {
            # Lexically scoped
            my str $sigil := self.sigil;
            if $sigil eq '$' && (my int $prim-spec := nqp::objprimspec($of)) {
                # Natively typed; just declare it.
                my $qast := QAST::Var.new(
                    :scope($!is-rw ?? 'lexicalref' !! 'lexical'), :decl('var'), :name(self.name),
                    :returns($of)
                );
                if $!is-parameter || $!initializer {
                    $qast
                }
                else {
                    my $init;
                    my $assign-op := 'bind';
                    if $prim-spec == 1 || (4 <= $prim-spec && $prim-spec <= 6) {
                        $init := QAST::IVal.new( :value(0) );
                    }
                    elsif $prim-spec == 1 || (7 <= $prim-spec && $prim-spec <= 10) {
                        $init := QAST::IVal.new( :value(0) );
                    }
                    elsif $prim-spec == 2 {
                        $init := QAST::NVal.new( :value(0e0) );
                    }
                    else {
                        $init := QAST::SVal.new( :value('') );
                    }
                    QAST::Op.new( :op($assign-op), $qast, $init )
                }
            }
            elsif $!initializer && $!initializer.is-binding {
                # Will be bound on first use, so just a declaration.
                QAST::Var.new( :scope('lexical'), :decl('var'), :name(self.name) )
            }
            else {
                # Need to vivify the object. Note: maybe we want to drop the
                # contvar, though we'll need an alternative for BEGIN.
                my $container := self.meta-object;
                $context.ensure-sc($container);
                my $qast := QAST::Var.new(
                    :scope('lexical'), :decl('contvar'), :name(self.name),
                    :value($container)
                );
                if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
                    my $value := self.IMPL-CONTAINER-TYPE($of);
                    $context.ensure-sc($value);
                    $qast := QAST::Op.new( :op('bind'), $qast, QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :$value )
                    ) );
                }
                $qast
            }
        }
        elsif $scope eq 'our' || $!desigilname.is-multi-part {
            # Package scoped lexical alias. We want to bind the lexical to
            # a lookup in the package.
            my $container := self.meta-object;
            $context.ensure-sc($container);
            my $lookup := $!desigilname.IMPL-QAST-PACKAGE-LOOKUP($context, $!package, :sigil($!sigil), :global-fallback);
            $lookup.name('VIVIFY-KEY');
            QAST::Op.new(
              :op('bind'),
              QAST::Var.new( :scope('lexical'), :decl('contvar'), :name(self.name), :returns($of), :value($container) ),
              $lookup
            )
        }
        elsif $scope eq 'has' || $scope eq 'HAS' {
            # No declaration to install
            QAST::Op.new( :op('null') )
        }
        elsif $scope eq 'state' {
            # Lexically scoped state variable
            my str $sigil := self.sigil;
            if $sigil eq '$' && nqp::objprimspec($of) {
                nqp::die("Natively typed state variables not yet implemented");
            }
            elsif $!initializer && $!initializer.is-binding {
                # Will be bound on first use, so just a declaration.
                QAST::Var.new(:scope('lexical'), :decl('var'), :name(self.name))
            }
            else {
                # Need to vivify the object.
                my $container := self.meta-object;
                $context.ensure-sc($container);
                QAST::Var.new(
                    :scope('lexical'), :decl('statevar'), :name(self.name),
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
        my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my str $name := self.name;
        my $qast;
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            my str $sigil := self.sigil;
            my $var-access := QAST::Var.new( :$name, :scope<lexical> );
            my $of := self.IMPL-OF-TYPE;

            if $sigil eq '$' && (my int $prim-spec := nqp::objprimspec($of)) {
                # Natively typed value. Need to initialize it to a default
                # in the absence of an initializer.
                my $init;
                my $assign-op := 'bind';
                $var-access.scope('lexicalref');
                if $prim-spec == 1 || (4 <= $prim-spec && $prim-spec <= 6) {
                    $assign-op := 'assign_i';
                    $var-access.returns(int);
                    $init := QAST::IVal.new( :value(0) );
                }
                elsif $prim-spec == 1 || (7 <= $prim-spec && $prim-spec <= 10) {
                    $assign-op := 'assign_u';
                    $var-access.returns(uint);
                    $init := QAST::IVal.new( :value(0) );
                }
                elsif $prim-spec == 2 {
                    $assign-op := 'assign_n';
                    $var-access.returns(num);
                    $init := QAST::NVal.new( :value(0e0) );
                }
                else {
                    $assign-op := 'assign_s';
                    $var-access.returns(str);
                    $init := QAST::SVal.new( :value('') );
                }
                if $!initializer {
                    if nqp::istype($!initializer, RakuAST::Initializer::Assign) {
                        $init := $!initializer.expression.IMPL-TO-QAST($context);
                    }
                    else {
                        nqp::die('Can only compile an assign initializer on a native');
                    }
                    $qast := QAST::Op.new( :op($assign-op), $var-access, $init )
                }
                else {
                    $qast := $var-access
                }
            }

            else {
                my $bind-constraint := self.IMPL-BIND-CONSTRAINT($of);
                if $bind-constraint.HOW.archetypes($bind-constraint).generic {
                    $var-access := QAST::Op.new(
                        :op('callmethod'), :name('instantiate_generic'),
                        QAST::Op.new( :op('p6var'), $var-access ),
                        QAST::Op.new( :op('curlexpad') ));
                }

                if $sigil eq '@' && $!shape {
                    my $value := self.IMPL-CONTAINER-TYPE($of);
                    $context.ensure-sc($value);
                    $var-access := QAST::Op.new( :op('bind'), $var-access, QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :$value )
                    ) );
                    my $shape_ast := $!shape.IMPL-TO-QAST($context);
                    $shape_ast.named('shape');
                    $var-access[1].push($shape_ast);
                }

                # Reference type value with an initializer
                if $!initializer {
                    my $init-qast := $!initializer.IMPL-TO-QAST($context, :invocant-qast($var-access));
                    my $perform-init-qast;
                    if $!initializer.is-binding {
                        my $source := $sigil eq '@' || $sigil eq '%'
                          ?? QAST::Op.new( :op('decont'), $init-qast)
                          !! $init-qast;
                        my $type := self.bind-constraint;
                        $context.ensure-sc($type);
                        if !nqp::eqaddr($type, Mu) {
                            $source := QAST::Op.new(
                                :op('p6bindassert'),
                                $source, QAST::WVal.new( :value($type) ))
                        }
                        $perform-init-qast := QAST::Op.new(
                          :op('bind'), $var-access, $source
                        );
                    }
                    else {
                        # Assignment. Case-analyze by sigil.
                        if $sigil eq '@' || $sigil eq '%' {
                            # Call STORE method, passing :INITIALIZE to indicate
                            # it's the initialization for immutable types.
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
                        $qast := QAST::Op.new(
                          :op('if'),
                          QAST::Op.new( :op('p6stateinit') ),
                          $perform-init-qast,
                          $var-access
                        )
                    }
                    else {
                        $qast := $perform-init-qast;
                    }
                }

                # Just a declaration; compile into an access to the variable.
                else {
                    $qast := $var-access
                }
            }
        }
        elsif $scope eq 'has' || $scope eq 'HAS' {
            # These just evaluate to Nil
            $qast := $lookups[1].IMPL-TO-QAST($context)
        }
        else {
            nqp::die("Don't know how to compile initialization for scope $scope");
        }

        my $thunks := self.outer-most-thunk;
        if $thunks {
            $thunks.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'), :expression(RakuAST::Expression::QAST.new($qast)));
            $thunks.IMPL-THUNK-VALUE-QAST($context)
        }
        else {
            $qast
        }
    }

    method IMPL-ATTRIBUTE-NAME() {
        $!sigil ~ '!' ~ $!desigilname.canonicalize
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $scope := self.scope;
        if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
            my str $scope := 'lexical';
            unless $rvalue {
                # Potentially l-value native lookups need a lexicalref.
                if self.sigil eq '$' && self.scope ne 'our' {
                    my $of := self.IMPL-OF-TYPE;
                    if nqp::objprimspec($of) && (!$!is-parameter || !$!is-ro) {
                        $scope := 'lexicalref';
                    }
                    return QAST::Var.new( :name(self.name), :$scope, :returns($of) );
                }
            }
            QAST::Var.new( :name(self.name), :$scope )
        }
        elsif self.is-attribute {
            my $package := $!attribute-package.meta-object;
            my $name := self.IMPL-ATTRIBUTE-NAME;
            if $package.HOW.has_attribute($package, $name) {
                my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
                $context.ensure-sc($package);
                return QAST::Var.new(
                    :scope(nqp::objprimspec($attr-type) ?? 'attributeref' !! 'attribute'),
                    :name($name), :returns($attr-type),
                    QAST::Var.new(:scope('lexical'), :name('self')),
                    QAST::WVal.new( :value($package) )
                )
            }
            QAST::Op.new(:op<null>)
        }
        else {
            nqp::die("Cannot compile lookup of scope $scope")
        }
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        my str $scope := self.scope;
        my $native := nqp::objprimspec(self.IMPL-OF-TYPE);
        nqp::die('Can only compile bind to my-scoped variables') unless $scope eq 'my' || $scope eq 'state' || $scope eq 'our';
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name(self.name), :scope($native && $!is-rw ?? 'lexicalref' !! 'lexical') ),
            $source-qast
        )
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method needs-sink-call() { False }

    method dump-markers() {
        '【' ~ self.name ~ '】'
    }
}

# Subclass to mark that the declaration was automatically generated.
# Used in correctly raku-fying and deparsing
class RakuAST::VarDeclaration::Auto
  is RakuAST::VarDeclaration::Simple { }

class RakuAST::VarDeclaration::Signature
  is RakuAST::Declaration
  is RakuAST::ImplicitLookups
  is RakuAST::TraitTarget
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Term
{
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
        self.visit-traits($visitor);
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

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $traits := self.IMPL-UNWRAP-LIST(self.traits);
        my str $scope := self.scope;
        my $type := $!type;
        for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
            # tell the parameter targets to create containers
            if $type {
                $param.target.set-type($type);
            }
            if $param.target {
                $param.target.replace-scope($scope);
                for $traits {
                    $param.target.add-trait(nqp::clone($_));
                }
            }
        }
        if $scope eq 'has' || $scope eq 'HAS' {
            my $attribute-package := $resolver.find-attach-target('package');
            if $attribute-package {
                nqp::bindattr(self, RakuAST::VarDeclaration::Signature, '$!attribute-package',
                    $attribute-package);
                for self.IMPL-UNWRAP-LIST(self.signature.parameters) {
                    #TODO this should probably live in the target's attach method
                    if $_.target && nqp::istype($_.target, RakuAST::ParameterTarget::Var) {
                        $_.target.replace-scope($scope);
                        nqp::bindattr($_.target, RakuAST::ParameterTarget::Var, '$!attribute-package',
                            $attribute-package);
                    }
                }
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

        my $binding := self.initializer && self.initializer.is-binding;
        for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
            $param.set-bindable(False) if $binding;
            $param.set-default-rw unless $binding;
            for $traits {
                $param.target.replace-scope($scope);
                $param.target.add-trait(nqp::clone($_)) if $param.target;
            }
        }
        self.signature.to-begin-time($resolver, $context);
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        unless $!initializer {
            for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
                if nqp::istype($param.target, RakuAST::ParameterTarget::Term) {
                    self.add-sorry($resolver.build-exception: 'X::Syntax::Term::MissingInitializer');
                    last;
                }
            }
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::isconcrete($!initializer) {
            my $perform-init-qast;
            my $value-list   := QAST::Op.new( :op('call'), :name('&infix:<,>') );
            my @params := self.IMPL-UNWRAP-LIST($!signature.parameters);
            my @terms;

            for @params {
                nqp::push(@terms, $_.target) if nqp::istype($_.target, RakuAST::ParameterTarget::Term);
                $value-list.push: $_.target.IMPL-LOOKUP-QAST($context) if $_.target;
            }
            if nqp::istype($!initializer, RakuAST::Initializer::Assign) {
                my $init-qast := $!initializer.IMPL-TO-QAST($context);
                my $list := QAST::Op.new( :op('p6store'), $value-list, $init-qast);
                if 0 < nqp::elems(@terms) {
                    my $stmts := QAST::Stmts.new(:resultchild(0), $list);
                    for @terms {
                        $stmts.push(
                            $_.IMPL-BIND-QAST(
                                $context,
                                QAST::Op.new(:op('decont'), $_.IMPL-LOOKUP-QAST($context))
                            )
                        );
                    }
                    $perform-init-qast := $stmts;
                }
                else {
                    $perform-init-qast := $list;
                }
            }
            elsif nqp::istype($!initializer, RakuAST::Initializer::Bind) {
                my $signature := $!signature.meta-object;
                $context.ensure-sc($signature);
                my $init-qast := $!initializer.IMPL-TO-QAST($context);
                $perform-init-qast := QAST::Op.new(
                    :op('p6bindcaptosig'),
                    QAST::WVal.new( :value($signature) ),
                    QAST::Op.new(
                        :op('callmethod'), :name('Capture'),
                        $init-qast
                    )
                );
            }
            else {
                nqp::die('Not yet supported signature initializer: ' ~ $!initializer.HOW.name($!initializer));
            }
            if self.scope eq 'state' {
                $perform-init-qast := QAST::Op.new(
                  :op('if'),
                  QAST::Op.new( :op('p6stateinit') ),
                  $perform-init-qast,
                  $value-list
                )
            }
            return $perform-init-qast;
        }

        QAST::Op.new(:op<null>);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-TO-QAST($context)
    }

    method needs-sink-call() { False }
}

# An anonymous variable declaration, such as `my $ = 42`
class RakuAST::VarDeclaration::Anonymous
  is RakuAST::VarDeclaration::Simple
{
    method new(str :$sigil!, str :$twigil, RakuAST::Type :$type, RakuAST::Initializer :$initializer,
               List :$traits, RakuAST::SemiList :$shape, str :$scope, Bool :$is-parameter,
               RakuAST::Expression :$where) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!desigilname',
            self.IMPL-GENERATE-NAME());
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!sigil', $sigil);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Simple, '$!twigil', $twigil);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!shape',
          $shape // RakuAST::SemiList);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!initializer',
            $initializer // RakuAST::Initializer);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-parameter',
          $is-parameter ?? True !! False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!where',
          $where // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!original-type',
          $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-rw', False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple, '$!is-bindable', True);
        $obj
    }

    method IMPL-GENERATE-NAME() {
        RakuAST::Name.from-identifier(QAST::Node.unique('ANON_VAR'))
    }

    method desigilname() {
        RakuAST::Name.from-identifier('')
    }

    method generate-lookup() {
        nqp::die('Cannot generate lookup of an anonymous variable');
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'state'])
    }

    method IMPL-BIND-QAST(
      RakuAST::IMPL::QASTContext $context,
                      QAST::Node $source-qast
    ) {
        $source-qast
    }
}

class RakuAST::VarDeclaration::AttributeAlias
  is RakuAST::VarDeclaration
{
    has RakuAST::Name $.desigilname;
    has str $.sigil;
    has RakuAST::VarDeclaration::Simple $.attribute;

    method new(RakuAST::Name :$desigilname!, str :$sigil!, RakuAST::VarDeclaration::Simple :$attribute!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::AttributeAlias, '$!desigilname', $desigilname);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::AttributeAlias, '$!sigil', $sigil);
        nqp::bindattr($obj, RakuAST::VarDeclaration::AttributeAlias, '$!attribute', $attribute);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!desigilname);
    }

    method generate-lookup() {
        RakuAST::Var::Attribute.new(self.IMPL-ATTRIBUTE-NAME);
    }

    method lexical-name() {
        $!sigil ~ $!desigilname.canonicalize
    }

    method default-scope() {
        'my'
    }

    method can-be-bound-to() {
        # stubbed-meta-object gives you the type object without trying to compose it first.
        my $package := $!attribute.attribute-package.stubbed-meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            return nqp::objprimspec($attr-type) ?? False !! True
        }

        # Return True so we don't have multi error for a non-existent attribute.
        True
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<null>)
    }

    method IMPL-ATTRIBUTE-NAME() {
        $!sigil ~ '!' ~ $!desigilname.canonicalize
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        my $package := $!attribute.attribute-package.meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            $context.ensure-sc($package);
            return QAST::Var.new(
                :scope(nqp::objprimspec($attr-type) ?? 'attributeref' !! 'attribute'),
                :name($name), :returns($attr-type),
                QAST::Var.new(:scope('lexical'), :name('self')),
                QAST::WVal.new( :value($package) ),
            )
        }
        QAST::Op.new(:op<null>)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        my $package := $!attribute.attribute-package.meta-object;
        my $name := self.IMPL-ATTRIBUTE-NAME;
        if $package.HOW.has_attribute($package, $name) {
            my $attr-type := $package.HOW.get_attribute_for_usage($package, $name).type;
            unless nqp::eqaddr($attr-type, Mu) {
                $context.ensure-sc($attr-type);
                $source-qast := QAST::Op.new(
                    :op('p6bindassert'),
                    $source-qast,
                    QAST::WVal.new( :value($attr-type) )
                );
            }
            return QAST::Op.new(
                :op('bind'),
                QAST::Var.new(
                    :scope('attribute'), :name($name), :returns($attr-type),
                    QAST::Var.new(:scope('lexical'), :name('self')),
                    QAST::WVal.new( :value($package) ),
                ),
                $source-qast
            )
        }
        QAST::Op.new(:op<null>)
    }
}

# The declaration of a term (sigilless) variable.
class RakuAST::VarDeclaration::Term
  is RakuAST::VarDeclaration
  is RakuAST::Term
{
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

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!initializer.add-colonpair($pair);
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name.canonicalize) )
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $invocant := nqp::defined($!type)
            ?? RakuAST::Type.IMPL-MAYBE-NOMINALIZE($!type.meta-object)
            !! Mu;
        $context.ensure-sc($invocant);
        my $invocant-qast := QAST::WVal.new(:value($invocant));
        my $init-qast := $!initializer.IMPL-TO-QAST($context, :$invocant-qast);
        if $!type && !$!type.is-known-to-be-exactly(Mu) {
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
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
class RakuAST::VarDeclaration::Implicit
  is RakuAST::VarDeclaration
{
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
class RakuAST::VarDeclaration::Implicit::Special
  is RakuAST::VarDeclaration::Implicit
  is RakuAST::Meta
{
    method PRODUCE-META-OBJECT() {
        # Reuse the container descriptor for the common cases that we expect
        # to have.
        my constant COMMON := nqp::list(
            nqp::hash(),
            nqp::hash(  # 6.c
                '$_', ContainerDescriptor.new(:of(Mu), :default(Any), :dynamic, :name('$_')),
                '$/', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            ),
            nqp::hash(  # 6.d
                '$_', ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :name('$_')),
                '$/', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            ),
            nqp::hash(  # 6.e
                '$_', ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :name('$_')),
                '$/', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            )
        );
        my $cont-desc := COMMON[nqp::getcomp('Raku').language_revision]{self.name} //
            ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :name(self.name));
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $cont-desc);
        nqp::bindattr($container, Scalar, '$!value', $cont-desc.default);
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

    method can-be-bound-to() { True }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        QAST::Op.new(
          :op('bind'),
          QAST::Var.new( :name(self.name), :scope('lexical') ),
          $source-qast
        )
    }
}

# Implicit block topic declaration. By default it is an optional parameter,
# but can be configured to be either a non-parameter (always from outer), a
# required parameter, or to obtain the current exception (for when it is a
# CATCH or CONTROL block);
class RakuAST::VarDeclaration::Implicit::BlockTopic
  is RakuAST::VarDeclaration::Implicit
{
    has Bool $.parameter;
    has Bool $.required;
    has Bool $.exception;
    has Bool $.loop;

    method new(Bool :$parameter, Bool :$required, Bool :$exception, Bool :$loop) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '$_');
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!parameter',
            $parameter // True);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!required',
            $required // False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!exception',
            $exception // False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!loop',
            $loop // False);
        $obj
    }

    method IMPL-NOT-IF-DUPLICATE() {
        $!loop
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
class RakuAST::VarDeclaration::Implicit::Constant
  is RakuAST::VarDeclaration::Implicit
  is RakuAST::TraitTarget
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Meta
  is RakuAST::CompileTimeValue
  is RakuAST::Declaration::Mergeable
{
    has Mu $.value;

    method new(str :$name!, Mu :$value! is raw, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        $obj
    }

    method set-value($value is raw) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
    }

    method compile-time-value() { $!value }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $context.ensure-sc($!value);
        QAST::Var.new( :decl('static'), :scope('lexical'), :name(self.name), :value($!value) )
    }

    method PRODUCE-META-OBJECT() {
        self.compile-time-value
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self, :SYMBOL(RakuAST::StrLiteral.new(self.name)));
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;
    }
}

# An enum value, e.g. "a" and "b" from enum Foo <a b>
# Is just a constant but we're using the subclass to identify them and
# apply special handling on conflicts.
class RakuAST::VarDeclaration::Implicit::EnumValue
  is RakuAST::VarDeclaration::Implicit::Constant
{
}

# An implicitly declared block (like an auto-generated proto)
class RakuAST::VarDeclaration::Implicit::Block
  is RakuAST::VarDeclaration
  is RakuAST::CheckTime
{
    has Mu $.block;

    method new(Mu :$block!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Block, '$!block', $block);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::can($!block, 'sort_dispatchees') {
            $!block.sort_dispatchees();
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $!block.IMPL-QAST-DECL-CODE($context);
    }

    method lexical-name() { '' }
}

# The implicit `self` term declaration for the invocant.
class RakuAST::VarDeclaration::Implicit::Self
  is RakuAST::VarDeclaration::Implicit
{
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
class RakuAST::VarDeclaration::Implicit::Cursor
  is RakuAST::VarDeclaration::Implicit
{
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

# We install regex captures in the lexpad and look it up when we need it.
# This means we can avoid closure-cloning it per time we enter it, for example
# if it is quantified.
class RakuAST::VarDeclaration::Implicit::RegexCapture
  is RakuAST::VarDeclaration::Implicit
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', QAST::Node.unique('!__REGEX_CAPTURE_'));
        $obj;
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(
            :scope('lexical'), :decl('var'), :name(self.name),
        )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        QAST::Op.new(
          :op('bind'),
          QAST::Var.new( :name(self.name), :scope('lexical') ),
          $source-qast
        )
    }
}

# The implicit `&?ROUTINE` term declaration for the routine.
class RakuAST::VarDeclaration::Implicit::Routine
  is RakuAST::VarDeclaration::Implicit
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '&?ROUTINE');
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        # We cannot just put the AST node's meta-object into a WVal for this, because
        # we may be running a clone of that object.
        QAST::Op.new:
            :op('bind'),
            QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) ),
            QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
    }
}

# Used for constructs that generate state variables
class RakuAST::VarDeclaration::Implicit::State
  is RakuAST::VarDeclaration::Implicit
  is RakuAST::ImplicitLookups
  is RakuAST::Meta
{
    has int $!init-to-zero;

    method new(str $name, int :$init-to-zero) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'state');
        nqp::bindattr_i($obj, RakuAST::VarDeclaration::Implicit::State, '$!init-to-zero', $init-to-zero // 0);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups := $!init-to-zero ?? [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Int'))
        ] !! [];
        self.IMPL-WRAP-LIST(@lookups)
    }

    method PRODUCE-META-OBJECT() {
        my str $name := nqp::getattr_s(self, RakuAST::VarDeclaration::Implicit, '$!name');

        # Create a container descriptor and attach it to a Scalar container, then set it to Int.new(0)
        my $descriptor := ContainerDescriptor.new(:of(Mu), :default(Any), :!dynamic, :$name);
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $descriptor);
        if $!init-to-zero {
            my $Int := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value;
            nqp::bindattr($container, Scalar, '$!value', nqp::box_i(0, $Int));
        } else {
            nqp::bindattr($container, Scalar, '$!value', $descriptor.default);
        }

        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new: :decl<statevar>, :scope<lexical>, :name(self.name), :value($container)
    }
}

# commonalities for doc variables
class RakuAST::VarDeclaration::Implicit::Doc
  is RakuAST::VarDeclaration::Implicit
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has Mu $.value;
    has Mu $!cu;

    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name',
          self.name);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!cu',
          $resolver.find-attach-target('compunit'));
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::Op.new:
          :op('bind'),
          QAST::Var.new(:decl('static'), :scope('lexical'), :name(
            nqp::getattr_s(self, RakuAST::VarDeclaration::Implicit, '$!name')
          )),
          QAST::WVal.new(:$value);
    }
}

# The implicit `$=pod` term declaration for legacy pod access
class RakuAST::VarDeclaration::Implicit::Doc::Pod
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=pod' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          nqp::getattr(
            self,RakuAST::VarDeclaration::Implicit::Doc,'$!cu'
          ).pod-content
        );
    }
}

# The implicit `$=data` term declaration for =data access
class RakuAST::VarDeclaration::Implicit::Doc::Data
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=data' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          nqp::ifnull(
            nqp::getattr(
              self,RakuAST::VarDeclaration::Implicit::Doc,'$!cu'
            ).data-content,
            Mu
          )
        );
    }
}

# The implicit `$=finish` term declaration for trailing documentation
class RakuAST::VarDeclaration::Implicit::Doc::Finish
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=finish' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          nqp::getattr(
            self,RakuAST::VarDeclaration::Implicit::Doc,'$!cu'
          ).finish-content,
        );
    }
}

# The implicit `$=rakudoc` term declaration for RakuDoc access
class RakuAST::VarDeclaration::Implicit::Doc::Rakudoc
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=rakudoc' }

    method fetch-blocks(RakuAST::StatementList $statement-list) {
        for nqp::getattr(
          $statement-list,RakuAST::StatementList,'$!statements'
        ) {
            if nqp::istype($_,RakuAST::Doc::Block) {
                nqp::push($*RAKUDOC,$_);
            }
            elsif nqp::istype($_,RakuAST::Statement::Expression) {
                my $expression := $_.expression;
                if nqp::istype($expression,RakuAST::Doc::DeclaratorTarget)
                  && $expression.WHY {
                    nqp::push($*RAKUDOC,$expression);
                }
            }
            elsif nqp::istype($_,RakuAST::Blockoid) {
                self.fetch-blocks($_.statement-list);
            }
        }
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $*RAKUDOC := [];
        self.fetch-blocks(
          nqp::getattr(
            self,RakuAST::VarDeclaration::Implicit::Doc,'$!cu'
          ).statement-list
        );

        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          self.IMPL-WRAP-LIST($*RAKUDOC)
        );
    }
}

# The commonalities for placeholder parameters.
class RakuAST::VarDeclaration::Placeholder
  is RakuAST::VarDeclaration
  is RakuAST::Term
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has Bool $!already-declared;

    method lexical-name() { nqp::die('Missing lexical-name implementation') }

    method generate-parameter() {
        nqp::die('Missing generate-parameter implementation')
    }

    method default-scope() { 'my' }

    method allowed-scopes() { ['my'] }

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new(self.lexical-name);
        $lookup.set-resolution(self);
        $lookup
    }

    method is-simple-lexical-declaration() {
        False
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $owner := $resolver.find-attach-target('block');
        my $method := $resolver.find-attach-target('method');
        if $owner {
            $owner.add-placeholder-parameter(self);
            $owner.add-generated-lexical-declaration(self)
                unless $!already-declared || self.lexical-name eq '%_' && $method;
        }
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $signature := $resolver.find-attach-target('block').signature;
        my $method := $resolver.find-attach-target('method');
        if $signature && $signature.parameters-initialized {
            # @_ and %_ are only real placeholders if they were not
            # already defined in the signature, so we need to check
            # there before pulling the plug
            my $name := self.lexical-name;
            if $name eq '@_' || $name eq '%_' {
                return True if $signature.IMPL-HAS-PARAMETER($name) || $name eq '%_' && $method;
            }
            self.add-sorry:
              $resolver.build-exception: 'X::Signature::Placeholder',
                precursor   => 1,
                placeholder => $name;
            return False;
        }
        True
    }

    method IMPL-ALREADY-DECLARED(Bool $declared) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Placeholder, '$!already-declared', $declared ?? True !! False);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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
class RakuAST::VarDeclaration::Placeholder::Positional
  is RakuAST::VarDeclaration::Placeholder
{
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
          target => RakuAST::ParameterTarget::Var.new(:name(self.lexical-name))
    }
}

# A named placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::Named
  is RakuAST::VarDeclaration::Placeholder
{
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
        my str $name := self.lexical-name;
        RakuAST::Parameter.new:
          target   => RakuAST::ParameterTarget::Var.new(:$name),
          names    => [nqp::substr($name,1)],
          optional => 0
    }
}

# base class for slurpy placeholders
class RakuAST::VarDeclaration::Placeholder::Slurpy
  is RakuAST::VarDeclaration::Placeholder
{
    method new() {
        nqp::create(self)
    }

    method generate-parameter() {
        RakuAST::Parameter.new:
          target => RakuAST::ParameterTarget::Var.new(:name(self.lexical-name)),
          slurpy => RakuAST::Parameter::Slurpy::Flattened
    }
}

# A slurpy array placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyArray
  is RakuAST::VarDeclaration::Placeholder::Slurpy
{
    method lexical-name() { '@_' }
}

# A slurpy hash placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyHash
  is RakuAST::VarDeclaration::Placeholder::Slurpy
{
    method lexical-name() { '%_' }
}
