# Base class for doc declarators
class RakuAST::Doc::Declarator
  is RakuAST::Doc
  does RakuAST::CheckTime
{
    has RakuAST::Doc::DeclaratorTarget $.WHEREFORE;
    has List                           $.leading;
    has List                           $.trailing;
    has int                            $!pod-index;
    has List                           $!paragraphs;
    has Mu                             $!pod;
    has Mu                             $!documented;

    method new(:$WHEREFORE, :$leading, :$trailing) {
        my $obj := nqp::create(self);
        $obj.set-WHEREFORE($WHEREFORE);
        $obj.set-leading($leading);
        $obj.set-trailing($trailing);

        # A target that does not surface its documentation in $=pod must
        # not reserve a position there, as it would never be filled in.
        if nqp::isconcrete($*LEGACY-POD-INDEX)
          && (!nqp::isconcrete($WHEREFORE) || $WHEREFORE.podifiable) {
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

    method IMPL-DOCUMENTABLE(Mu $meta) {
        !nqp::eqaddr($meta, Mu) && $meta.HOW.name($meta) ne 'Any'
    }

    # Sets the documentation as the .WHY of the given meta-object. Doing
    # so again for the same meta-object refills the Pod::Block::Declarator
    # made the first time, so code holding it sees documentation added since.
    method IMPL-DOCUMENT(Mu $meta) {
        my $pod := nqp::eqaddr($meta, $!documented)
          ?? self.podify($meta, $!pod)
          !! self.podify($meta);
        nqp::bindattr(self, RakuAST::Doc::Declarator, '$!pod', $pod);
        nqp::bindattr(self, RakuAST::Doc::Declarator, '$!documented', $meta);
        $pod
    }

    # Takes the documentation back off the meta-object it was set on, as
    # when a subset takes back a doc a declarand in its where clause took.
    method IMPL-UNDOCUMENT() {
        if nqp::isconcrete($!pod) {
            my $meta := $!documented;
            if nqp::isconcrete($meta) {
                my $class := nqp::istype($meta, Parameter)
                  ?? Parameter
                  !! nqp::istype($meta, Attribute) ?? Attribute !! Block;
                nqp::bindattr($meta, $class, '$!why', nqp::null);
            }
            else {
                $meta.HOW.set_why(NQPMu);
            }
            nqp::bindattr(self, RakuAST::Doc::Declarator, '$!pod', Mu);
            nqp::bindattr(self, RakuAST::Doc::Declarator, '$!documented', Mu);
        }
        Nil
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver,
                RakuAST::IMPL::QASTContext $context) {
        if $!WHEREFORE {
            if $!WHEREFORE.podifiable {
                my $meta := $!WHEREFORE.IMPL-DOC-META-OBJECT($resolver, $context);
                if self.IMPL-DOCUMENTABLE($meta) {
                    $resolver.find-attach-target('compunit').set-pod-content(
                      $!pod-index, self.IMPL-DOCUMENT($meta)
                    );
                }
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
role RakuAST::Doc::DeclaratorTarget {
    has RakuAST::Doc::Declarator $.WHY;
    has int $!documented-at-begin;

    # Whether the documentation on this target is surfaced through the
    # legacy pod system: turned into a Pod::Block::Declarator in $=pod
    # and set as the .WHY of the target's meta-object.  Targets without
    # a runtime meta-object to carry the documentation, such as lexical
    # variable declarations, return False: their documentation lives on
    # the AST node only and is available through $=rakudoc.
    method podifiable() { True }

    # The meta-object that carries the documentation at CHECK time, or Mu
    # when the target has none to carry it.
    method IMPL-DOC-META-OBJECT(RakuAST::Resolver $resolver,
                       RakuAST::IMPL::QASTContext $context) {
        self.meta-object
    }

    # Sets the documentation as the .WHY of the meta-object that traits
    # are applied to. Called at BEGIN time before the traits, so that they
    # and later BEGIN time code see it.
    method IMPL-DOCUMENT-AT-BEGIN() {
        nqp::bindattr_i(self, RakuAST::Doc::DeclaratorTarget,
          '$!documented-at-begin', 1);
        self.IMPL-UPDATE-WHY;
    }

    # Brings the .WHY set at BEGIN time up to date with documentation the
    # parser attached after it. Code adding docs after BEGIN must call this.
    method IMPL-UPDATE-WHY() {
        my $WHY := self.WHY;
        if $!documented-at-begin && $WHY && self.podifiable {
            my $meta := self.compile-time-value;
            $WHY.IMPL-DOCUMENT($meta) if $WHY.IMPL-DOCUMENTABLE($meta);
        }
        Nil
    }

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
        $WHY.IMPL-UNDOCUMENT if $WHY;
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
