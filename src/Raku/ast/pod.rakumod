# Base class for anything pod related
class RakuAST::Pod
  is RakuAST::Node { }

# Base class for pod declarators
class RakuAST::Pod::Declarator
  is RakuAST::Pod
{
    has RakuAST::Pod::DeclaratorTarget $.WHEREFORE;
    has List                           $.leading;
    has List                           $.trailing;

    method new(:$WHEREFORE, :$leading, :$trailing) {
        my $obj := nqp::create(self);
        $obj.set-WHEREFORE($WHEREFORE);
        $obj.set-leading($leading);
        $obj.set-trailing($trailing);
        $obj
    }

    method set-WHEREFORE(Mu $WHEREFORE) {
        nqp::bindattr(self, RakuAST::Pod::Declarator, '$!WHEREFORE',
          $WHEREFORE);
        Nil
    }
    method set-leading($leading) {
        nqp::bindattr(self, RakuAST::Pod::Declarator, '$!leading',
          $leading ?? self.IMPL-UNWRAP-LIST($leading) !! []);
        Nil
    }
    method set-trailing($trailing) {
        nqp::bindattr(self, RakuAST::Pod::Declarator, '$!trailing',
          $trailing ?? self.IMPL-UNWRAP-LIST($trailing) !! []);
        Nil
    }

    method leading()  { self.IMPL-WRAP-LIST($!leading)  }
    method trailing() { self.IMPL-WRAP-LIST($!trailing) }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
#        if $!WHEREFORE {
#            my $value := $!WHEREFORE.PRODUCE-META-OBJECT;
#            $context.ensure-sc($value);
#            QAST::WVal.new( :$value )
#        }
#        else {
            QAST::Op.new(:op<null>)
#        }
    }
}

# Role for objects that can have a Pod::Declarator attached
class RakuAST::Pod::DeclaratorTarget {
    has RakuAST::Pod::Declarator $!WHY;

    method set-WHY(RakuAST::Pod::Declarator $WHY) {
        nqp::bindattr(self, RakuAST::Pod::DeclaratorTarget, '$!WHY', $WHY);
        Nil
    }
}
