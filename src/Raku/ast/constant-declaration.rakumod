# A basic constant declaration of the form `my Type constant $foo = 42`
class RakuAST::VarDeclaration::Constant
  is RakuAST::Term
  does RakuAST::VarDeclaration
  does RakuAST::TraitTarget
  does RakuAST::CompileTimeValue
  does RakuAST::ImplicitLookups
  does RakuAST::Doc::DeclaratorTarget
  does RakuAST::BeginTime
{
    has str                  $.name;
    has RakuAST::Initializer $.initializer;
    has RakuAST::Type        $.type;
    has Mu                   $!value;
    has Mu                   $!package;

    method new(
                       str :$name!,
      RakuAST::Initializer :$initializer!,
                       str :$scope,
             RakuAST::Type :$type,
                      List :$traits
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,RakuAST::VarDeclaration::Constant,'$!name',
          $name
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Constant,'$!initializer',
          $initializer // RakuAST::Initializer  # XXX why default if required?
        );
        $obj.replace-scope($scope);
        nqp::bindattr($obj,RakuAST::VarDeclaration::Constant,'$!type',
          $type // RakuAST::Type
        );
        $obj.set-traits($traits) if $traits;
        $obj
    }

    method lexical-name()   { $!name }
    method sigil() { nqp::substr($!name,0,1) }
    method compile-time-value() { $!value }

    method default-scope()  { 'our'   }
    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our']) }
    method is-simple-lexical-declaration() { True }
    method needs-sink-call() { False }

    # A constant is its value, which has no meta-object of its own to
    # carry documentation at runtime
    method podifiable() { False }

    method visit-children(Code $visitor) {
        $visitor($!type)        if $!type;
        $visitor($!initializer) if $!initializer;
        self.visit-traits($visitor);
        $visitor(self.WHY)      if self.WHY;
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my str $sigil := self.sigil;
        [
          $sigil eq '@'
            ?? RakuAST::Type::Setting.from-identifier('Positional')
            !! $sigil eq '%'
              ?? RakuAST::Type::Setting.from-identifier('Associative')
              !! $sigil eq '&'
                ?? RakuAST::Type::Setting.from-identifier('Callable')
                !! ($!type || nqp::null)
        ]
    }

    method PERFORM-BEGIN(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $initializer := $!initializer;
        my str $sigil   := self.sigil;
        my $value;
        {
            CATCH {
                my $ex := $resolver.convert-exception($_);
                my $XUndeclaredSymbols := $resolver.resolve-name-constant-in-setting(
                  RakuAST::Name.from-identifier-parts(
                    'X', 'Undeclared', 'Symbols'
                  )
                );

                # The only reason for this distinction is that in the
                # legacy backend the grammar would have thrown such an
                # exception before we even tried to evaluate the constant
                # thus it would not have been wrapped in an X::Comp::BeginTime
                # and some spectests depend on this.
                if $XUndeclaredSymbols {
                    $ex := nqp::istype(
                      $ex,$XUndeclaredSymbols.compile-time-value
                    ) ?? $ex
                      !! $resolver.convert-begin-time-exception($ex);
                    self.IMPL-LOCATE-EXCEPTION($ex);
                    $ex.rethrow;
                }
                else {
                    nqp::rethrow($ex);
                }
            }

            # BEGIN-time evaluation of the initializer runs before the
            # CompUnit's CHECK-time sink propagation, so a nested
            # StatementList in the initializer would otherwise leave
            # non-final statements unsunk and a side-effecting expression
            # like a `while` loop would emit as a lazy Seq that gets
            # discarded without iterating. Propagate sink here as if the
            # initializer's value were wanted. A `.=` initializer carries
            # a method call rather than an expression and needs no sink
            # treatment.
            $initializer.expression.apply-sink(False)
              if nqp::istype($initializer, RakuAST::Initializer::Expression);

            $value := $initializer.IMPL-COMPILE-TIME-VALUE(
              $resolver, $context,
              :invocant-compiler(-> {
                  $!type ?? $!type.IMPL-VALUE-TYPE.meta-object !! Mu
              })
            );
        };
        $value := $value.Map
          if $sigil eq '%'
          && !nqp::istype($value, self.get-implicit-lookups[0].meta-object);
        $value := $value.cache
          if $sigil eq '@'
          && !nqp::istype($value, self.get-implicit-lookups[0].meta-object);
        nqp::bindattr(self,RakuAST::VarDeclaration::Constant,'$!value',$value);

        $initializer.IMPL-THUNK-EXPRESSION($resolver, $context);

        if self.scope eq 'our' {
            my $package := $resolver.current-package;

            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(
              self, RakuAST::VarDeclaration::Constant, '$!package', $package
            );

            # A same-scope redeclaration is caught by the lexical check, so a
            # name already present here is a nested shadow that overwrites.
            nqp::bindkey(
               $resolver.IMPL-STASH-HASH($package), $!name, $value
            );
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
        self.add-sorry: $resolver.build-exception('X::ParametricConstant')
          if $!type && ($sigil eq '@' || $sigil eq '%' || $sigil eq '&');

        my $type := self.get-implicit-lookups[0];
        if $type
          && !nqp::istype($!initializer, RakuAST::Initializer::CallAssign)
          && !nqp::objprimspec($type.meta-object) {
            unless nqp::istype($!value, $type.meta-object) {
                my $name := $!name || '<anon>';
                self.add-sorry(
                  $resolver.build-exception('X::Comp::TypeCheck',
                    operation => "constant declaration of $name",
                    expected  => $type.meta-object,
                    got       => $!value
                  )
                );
                return False;
            }
        }

        self.check-scope($resolver, 'constant');
        True
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my str $name := $!name;
        my $value    := $!value;

        $context.ensure-sc($value);
        my $constant := QAST::Stmts.new(
          $!initializer.IMPL-QAST-DECL($context),
          QAST::Var.new(:$name, :decl<static>, :scope<lexical>, :$value)
        );
        if self.scope eq 'our' {
            $context.ensure-sc($!package);
            QAST::Op.new(:op<bindkey>,
              QAST::Op.new(:op<who>,
                QAST::WVal.new(:value($!package))
              ),
              QAST::SVal.new(:value($name)),
              $constant
            )
        }
        else {
            $constant
        }
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::ifnull(
          self.IMPL-NATIVE-VALUE-QAST,
          QAST::Var.new(:name($!name), :scope<lexical>)
        )
    }

    # Return QAST for constant if QAST for a native value can be determined,
    # otherwise return nqp::null to indicate absence of native QAST
    method IMPL-NATIVE-VALUE-QAST() {
        if $!type {
            my int $primspec := nqp::objprimspec($!type.meta-object);
            if $primspec {
                my $value := nqp::decont($!value);
                $primspec == nqp::const::BIND_VAL_INT
                  || $primspec == nqp::const::BIND_VAL_UINT
                  ?? nqp::isbig_I($value)
                    ?? nqp::null   # too big for native int
                    !! QAST::IVal.new(:value(nqp::unbox_i($value)))
                  !! $primspec == nqp::const::BIND_VAL_STR
                    ?? QAST::SVal.new(:value(nqp::unbox_s($value)))
                    !! QAST::NVal.new(:value(nqp::unbox_n($value)))
            }

            # Not a native type
            else {
                nqp::null
            }
        }

        # No type info
        else {
            nqp::null
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }
}
