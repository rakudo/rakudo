# An anonymous variable declaration, such as `my $ = 42`
class RakuAST::VarDeclaration::Anonymous
  is RakuAST::VarDeclaration::Simple
{
    method new(
                       str :$scope,
             RakuAST::Type :$type,
                       str :$sigil!,
                       str :$twigil,
         RakuAST::SemiList :$shape,
                      List :$traits,
      RakuAST::Initializer :$initializer,
       RakuAST::Expression :$where,
                      Bool :$is-parameter
    ) {
        my $obj := nqp::create(self);
        $obj.replace-scope($scope);
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!type',
          $type // RakuAST::Type
        );
        nqp::bindattr_s($obj,RakuAST::VarDeclaration::Simple,'$!sigil',$sigil);
        nqp::bindattr_s($obj,RakuAST::VarDeclaration::Simple,'$!twigil',$twigil);

        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!shape',
          $shape // RakuAST::SemiList
        );
        $obj.set-traits($traits);
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!initializer',
          $initializer // RakuAST::Initializer
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!where',
          $where // RakuAST::Expression
        );
        nqp::bindattr($obj, RakuAST::VarDeclaration::Simple,'$!is-parameter',
          $is-parameter ?? True !! False
        );

        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!original-type',
          $type // RakuAST::Type
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!is-rw',
          False
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!is-bindable',
          True
        );
        nqp::bindattr($obj,RakuAST::VarDeclaration::Simple,'$!desigilname',
          RakuAST::Name.from-identifier(QAST::Node.unique('ANON_VAR'))
        );
        $obj
    }

    method desigilname() { RakuAST::Name.from-identifier('') }

    method generate-lookup() {
        nqp::die('Cannot generate lookup of an anonymous variable');
    }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'state']) }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {

        # Call the parent's method
        nqp::findmethod(RakuAST::VarDeclaration::Simple,'PERFORM-CHECK')(
          self, $resolver, $context
        );

        self.add-sunk-worry($resolver, 'unnamed ' ~ self.sigil ~ ' variable')
          if self.sunk && !self.initializer;
    }

    method IMPL-BIND-QAST(
      RakuAST::IMPL::QASTContext $context,
                      QAST::Node $qast
    ) {
        $qast
    }
}
