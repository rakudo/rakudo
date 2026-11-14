# The declaration of a term (sigilless) variable.
class RakuAST::VarDeclaration::Term
  is RakuAST::Term
  does RakuAST::VarDeclaration
  does RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Type $.type;
    has RakuAST::Name $.name;
    has RakuAST::Initializer $.initializer;
    has int $!lowered-to-local;
    has Mu $!lowered-away-sentinel;
    has str $!lowered-local-name;

    method new(str :$scope, RakuAST::Type :$type, RakuAST::Name :$name!,
            RakuAST::Initializer :$initializer) {
        my $obj := nqp::create(self);
        $obj.replace-scope($scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Term, '$!initializer',
            $initializer // RakuAST::Initializer);
        $obj
    }

    # The declaration is installed before its initializer is parsed, so the
    # term is visible within its own initializer (a self-referential lazy
    # binding such as `my \S := gather { ... S ... }`).
    method set-initializer(RakuAST::Initializer $initializer) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Term, '$!initializer', $initializer);
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

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Avoid worries about sink context
    }

    method IMPL-SET-LOWERED-TO-LOCAL(Mu $sentinel) {
        nqp::bindattr_i(self, RakuAST::VarDeclaration::Term, '$!lowered-to-local', 1);
        nqp::bindattr(self, RakuAST::VarDeclaration::Term, '$!lowered-away-sentinel', $sentinel);
    }

    # The frame-local name for a lowered term, or the empty string when
    # the term stays a by-name lexical.
    method IMPL-LOWERED-LOCAL-NAME() {
        return $!lowered-local-name if $!lowered-local-name;
        return '' unless $!lowered-to-local;
        return '' if nqp::isnull($!lowered-away-sentinel);
        nqp::bindattr_s(self, RakuAST::VarDeclaration::Term,
            '$!lowered-local-name',
            QAST::Node.unique('__lowered_' ~ $!name.canonicalize));
        if nqp::atkey(nqp::getenvhash(), 'RAKUDO_LOWERING_DEBUG') {
            RakuAST::IMPL::VarLowering.IMPL-NOTE(
                'lex2local: minted ' ~ $!lowered-local-name ~ ' for '
                    ~ $!name.canonicalize);
        }
        $!lowered-local-name
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if self.IMPL-LOWERED-LOCAL-NAME {
            # The by-name lexical stays declared for introspection.
            $context.ensure-sc($!lowered-away-sentinel);
            QAST::Stmts.new(
                QAST::Var.new(
                    :decl('static'), :scope('lexical'), :name($!name.canonicalize),
                    :value($!lowered-away-sentinel)
                ),
                QAST::Var.new(
                    :decl('var'), :scope('local'), :name($!lowered-local-name)
                )
            )
        }
        else {
            QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name.canonicalize) )
        }
    }

    # The declaration form for a scope flattened into its user's frame.
    # The local is cleared on every entry so a bind that a condition
    # skips does not leave the previous iteration's value readable.
    method IMPL-QAST-DECL-FLATTENED(RakuAST::IMPL::QASTContext $context) {
        my str $local-name := self.IMPL-LOWERED-LOCAL-NAME;
        nqp::die('Cannot emit a flattened declaration that is not lowered')
            unless $local-name;
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :scope('local'), :decl('var'), :name($local-name) ),
            QAST::Op.new( :op('null') )
        )
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $invocant := nqp::defined($!type)
            ?? $!type.meta-object
            !! Mu;
        # For `.=` initializers, the invocant must be the nominalized base
        # type (e.g. `my Int:D \b .= new: 42` should call `Int.new`, not
        # `Int:D.new`, since definite-constrained types are not instantiable).
        $invocant := RakuAST::Type.IMPL-MAYBE-NOMINALIZE($invocant)
            if nqp::istype($!initializer, RakuAST::Initializer::CallAssign);
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
        my str $local-name := self.IMPL-LOWERED-LOCAL-NAME;
        $local-name
            ?? QAST::Var.new( :name($local-name), :scope('local') )
            !! QAST::Var.new( :name($!name.canonicalize), :scope('lexical') )
    }

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }

    method needs-sink-call() { False }

    # a term declaration has no meta-object to carry documentation at
    # runtime
    method podifiable() { False }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
        $visitor($!name);
        $visitor($!initializer) if $!initializer;
        $visitor(self.WHY) if self.WHY;
    }
}
