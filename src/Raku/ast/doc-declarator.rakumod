# Base class for doc declarators
class RakuAST::Doc::Declarator
  is RakuAST::Doc
{
    has RakuAST::Doc::DeclaratorTarget $.WHEREFORE;
    has List                           $.leading;
    has List                           $.trailing;

    method new(:$WHEREFORE, :$leading, :$trailing) {
        my $obj := nqp::create(self);
        $obj.set-WHEREFORE($WHEREFORE);
        $obj.set-leading($leading);
        $obj.set-trailing($trailing);
        $obj
    }
    method visit-children(Code $visitor) {
        $visitor($!WHEREFORE) unless nqp::eqaddr($!WHEREFORE.WHY,self);
        $visitor($!leading)  if nqp::elems($!leading);
        $visitor($!trailing) if nqp::elems($!trailing);
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
}

# Role for objects that can have a Doc::Declarator attached
class RakuAST::Doc::DeclaratorTarget {
    has RakuAST::Doc::Declarator $.WHY;

    method set-WHY(RakuAST::Doc::Declarator $WHY) {
        if $WHY {
            nqp::bindattr(self, RakuAST::Doc::DeclaratorTarget, '$!WHY', $WHY);
            $WHY.set-WHEREFORE(self);
        }
        Nil
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
