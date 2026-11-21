# The declaration of a term (sigilless) variable.
class RakuAST::VarDeclaration::Term
  is RakuAST::Term
  does RakuAST::VarDeclaration
  does RakuAST::Doc::DeclaratorTarget
  does RakuAST::IMPL::Lowerable
{
    has RakuAST::Name        $.name;
    has RakuAST::Type        $.type;
    has RakuAST::Initializer $.initializer;

    method new(
             RakuAST::Name :$name!,
                       str :$scope,
             RakuAST::Type :$type,
      RakuAST::Initializer :$initializer
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj,RakuAST::VarDeclaration::Term,'$!name',
          $name
        );
        $obj.replace-scope($scope);
        nqp::bindattr($obj,RakuAST::VarDeclaration::Term,'$!type',
          $type // RakuAST::Type
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Term,'$!initializer',
          $initializer // RakuAST::Initializer
        );
        $obj
    }

    # The declaration is installed before its initializer is parsed, so the
    # term is visible within its own initializer (a self-referential lazy
    # binding such as `my \S := gather { ... S ... }`).
    method set-initializer(RakuAST::Initializer $initializer) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Term, '$!initializer',
          $initializer);
    }

    # A term declaration has no meta-object to carry documentation at
    # runtime
    method podifiable() { False }

    method lexical-name() { $!name.canonicalize }
    method default-scope() { 'my' }
    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }
    method needs-sink-call() { False }

    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!initializer.add-colonpair($pair);
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!type)        if $!type;
        $visitor($!initializer) if $!initializer;
        $visitor(self.WHY)      if self.WHY;
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        # XXX Avoid worries about sink context
    }

    method IMPL-GENERATE-LOWERED-NAME() {
        QAST::Node.unique('__lowered_' ~ self.lexical-name)
    }

    # The declaration form for a scope flattened into its user's frame.
    # The local is cleared on every entry so a bind that a condition
    # skips does not leave the previous iteration's value readable.
    method IMPL-QAST-DECL-FLATTENED(RakuAST::IMPL::QASTContext $context) {
        (my str $name := self.IMPL-LOWERED-LOCAL-NAME)
          ?? QAST::Op.new(:op<bind>,
               QAST::Var.new(:$name, :decl<var>, :scope<local>),
               QAST::Op.new(:op<null>)
             )
          !! nqp::die('Cannot emit a flattened declaration that is not lowered')
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $type     := $!type;
        my $invocant := nqp::defined($type) ?? $type.meta-object !! Mu;

        # For `.=` initializers, the invocant must be the nominalized base
        # type (e.g. `my Int:D \b .= new: 42` should call `Int.new`, not
        # `Int:D.new`, since definite-constrained types are not instantiable).
        $invocant := RakuAST::Type.IMPL-MAYBE-NOMINALIZE($invocant)
          if nqp::istype($!initializer, RakuAST::Initializer::CallAssign);

        $context.ensure-sc($invocant);
        my $init-qast := $!initializer.IMPL-TO-QAST(
          $context,
          :invocant-qast(QAST::WVal.new(:value($invocant)))
        );

        $init-qast := QAST::Op.new(:op<p6bindassert>,
          $init-qast,
          $type.IMPL-TO-QAST($context)
        ) if $type && !$type.is-known-to-be-exactly(Mu);

        QAST::Op.new(:op<bind>,
          self.IMPL-LOOKUP-QAST($context),
          $init-qast
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        (my str $local-name := self.IMPL-LOWERED-LOCAL-NAME)
          ?? QAST::Var.new(:name($local-name),       :scope<local>)
          !! QAST::Var.new(:name(self.lexical-name), :scope<lexical>)
    }
}
