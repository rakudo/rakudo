# Base class for doc declarators
class RakuAST::Doc::Declarator
  is RakuAST::Doc
  is RakuAST::CheckTime
{
    has RakuAST::Doc::DeclaratorTarget $.WHEREFORE;
    has List                           $.leading;
    has List                           $.trailing;
    has int                            $!pod-index;
    has List                           $!paragraphs;

    method new(:$WHEREFORE, :$leading, :$trailing) {
        my $obj := nqp::create(self);
        $obj.set-WHEREFORE($WHEREFORE);
        $obj.set-leading($leading);
        $obj.set-trailing($trailing);

        if nqp::isconcrete($*LEGACY-POD-INDEX) {
            nqp::bindattr_i($obj,RakuAST::Doc::Declarator,
              '$!pod-index', $*LEGACY-POD-INDEX++);
        }
        else {
            nqp::bindattr_i($obj,RakuAST::Doc::Declarator,'$!pod-index',-1);
        }

        $obj
    }
    method visit-children(Code $visitor) {
        $visitor($!WHEREFORE) unless nqp::eqaddr($!WHEREFORE.WHY,self);
    }

    method set-WHEREFORE(Mu $WHEREFORE) {
        nqp::bindattr(self, RakuAST::Doc::Declarator, '$!WHEREFORE',
          $WHEREFORE);
        Nil
    }

    method set-leading($leading) {
        nqp::bindattr(self, RakuAST::Doc::Declarator, '$!leading',
          $leading ?? self.IMPL-UNWRAP-LIST($leading) !! []);
        Nil
    }
    method add-leading($doc) { nqp::push($!leading, $doc) }
    method leading()  { self.IMPL-WRAP-LIST($!leading)  }

    method set-trailing($trailing) {
        nqp::bindattr(self, RakuAST::Doc::Declarator, '$!trailing',
          $trailing ?? self.IMPL-UNWRAP-LIST($trailing) !! []);
        Nil
    }
    method add-trailing($doc) { nqp::push($!trailing, $doc) }
    method trailing() { self.IMPL-WRAP-LIST($!trailing) }

    method PERFORM-CHECK(RakuAST::Resolver $resolver,
                RakuAST::IMPL::QASTContext $context) {
        if $!WHEREFORE {
            my $meta := $!WHEREFORE.meta-object;
            if $meta.HOW.name($meta) ne 'Any' {
                $resolver.find-attach-target('compunit').set-pod-content(
                  $!pod-index, self.podify($meta)
                );
            }
        }
        else {
            self.add-worry: $resolver.build-exception:
              'X::Syntax::Doc::Declarator::MissingDeclarand';
        }
        True
    }
}

# Role for objects that can have a Doc::Declarator attached
class RakuAST::Doc::DeclaratorTarget {
    has RakuAST::Doc::Declarator $.WHY;

    # A special method to create a a Declarator and connect it to the
    # target.  Intended to be used for a .raku representation
    method declarator-docs(:$leading, :$trailing) {
        nqp::bindattr(self, RakuAST::Doc::DeclaratorTarget, '$!WHY',
          RakuAST::Doc::Declarator.new(:WHEREFORE(self), :$leading, :$trailing)
        );
        self
    }

    method set-WHY(RakuAST::Doc::Declarator $WHY) {
        if $WHY {
            nqp::bindattr(self, RakuAST::Doc::DeclaratorTarget, '$!WHY', $WHY);
            $WHY.set-WHEREFORE(self);
        }
        Nil
    }

    method cut-WHY() {
        my $WHY := nqp::getattr(self,RakuAST::Doc::DeclaratorTarget,'$!WHY');
        nqp::bindattr(self,RakuAST::Doc::DeclaratorTarget,'$!WHY',
          RakuAST::Doc::Declarator);
        $WHY
    }

    method set-leading($doc) {
        (my $WHY := self.WHY)
          ?? $WHY.set-leading($doc)
          !! self.set-WHY(RakuAST::Doc::Declarator.new(
               WHEREFORE => self, leading => $doc
             ));
        Nil
    }

    method add-leading($doc) {
        (my $WHY := self.WHY)
          ?? $WHY.add-leading($doc)
          !! self.set-WHY(RakuAST::Doc::Declarator.new(
               WHEREFORE => self, leading => $doc
             ));
        Nil
    }

    method set-trailing($doc) {
        (my $WHY := self.WHY)
          ?? $WHY.set-trailing($doc)
          !! self.set-WHY(RakuAST::Doc::Declarator.new(
               WHEREFORE => self, trailing => $doc
             ));
        Nil
    }

    method add-trailing($doc) {
        (my $WHY := self.WHY)
          ?? $WHY.add-trailing($doc)
          !! self.set-WHY(RakuAST::Doc::Declarator.new(
               WHEREFORE => self, trailing => $doc
             ));
        Nil
    }
}
