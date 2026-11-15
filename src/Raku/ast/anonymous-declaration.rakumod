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
        $obj.replace-scope($scope);
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

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::findmethod(RakuAST::VarDeclaration::Simple, 'PERFORM-CHECK')(self, $resolver, $context);

        self.add-sunk-worry($resolver, 'unnamed ' ~ self.sigil ~ ' variable')
            if self.sunk && !self.initializer;
    }

    method IMPL-BIND-QAST(
      RakuAST::IMPL::QASTContext $context,
                      QAST::Node $source-qast
    ) {
        $source-qast
    }
}
