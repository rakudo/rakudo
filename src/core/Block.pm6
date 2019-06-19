my class PhasersList is repr('VMArray') {}
my class Block { # declared in BOOTSTRAP
    # class Block is Code
    #     has Mu $!phasers;
    #     has Mu $!why;

    method of(Block:D:)      { nqp::getattr(self,Code,'$!signature').returns }
    method returns(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method add_phaser(Str:D \name, &block --> Nil) {
        $!phasers := nqp::hash
          unless nqp::attrinited(self,Block,'$!phasers');

        my str $name = name;
        nqp::bindkey($!phasers,$name,nqp::create(PhasersList))
          unless nqp::existskey($!phasers,$name);

        if nqp::iseq_s($name,'LEAVE') || nqp::iseq_s($name,'KEEP') || nqp::iseq_s($name,'UNDO') {
            nqp::unshift(nqp::atkey($!phasers,$name),&block);
            self.add_phaser('!LEAVE-ORDER', &block);
        }
        elsif nqp::iseq_s($name,'NEXT') || nqp::iseq_s($name,'!LEAVE-ORDER') || nqp::iseq_s($name,'POST') {
            nqp::unshift(nqp::atkey($!phasers,$name),&block);
        }
        else {
            nqp::push(nqp::atkey($!phasers,$name),&block);
        }
    }

    method fire_if_phasers(Str $name --> Nil) {
        nqp::if(
          nqp::attrinited(self,Block,'$!phasers')
            && nqp::existskey($!phasers,$name),
          nqp::stmts(
            (my $iter := nqp::iterator(nqp::atkey($!phasers,$name))),
            nqp::while($iter,nqp::shift($iter)(),:nohandler)
          )
        )
    }

    method fire_phasers(Str $name --> Nil) {
        nqp::stmts(
          (my $iter := nqp::iterator(nqp::atkey($!phasers,$name))),
          nqp::while($iter,nqp::shift($iter)(),:nohandler)
        )
    }

    method has-phasers() { nqp::hllbool(nqp::attrinited(self,Block,'$!phasers')) }

    method has-phaser(Str:D \name) {
        nqp::hllbool(nqp::attrinited(self,Block,'$!phasers')
          && nqp::existskey($!phasers,nqp::unbox_s(name)))
    }

    method phasers(Str:D $name) {
        nqp::attrinited(self,Block,'$!phasers')
          && nqp::existskey($!phasers,nqp::unbox_s($name))
          ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
               nqp::atkey($!phasers,nqp::unbox_s($name)))
          !! ()
    }

    multi method perl(Block:D:) {
        "-> {self.signature.perl.substr(2,*-1)} \{ #`({self.WHICH}) ... \}"
    }

    method WHY() {
        if nqp::isnull($!why) {
            nextsame unless $*COMPILING_CORE_SETTING;
        } else {
            $!why.set_docee(self);
            $!why
        }
    }

    method set_why($why --> Nil) {
        $!why := $why;
    }

    # helper method for array slicing
    multi method POSITIONS(Block:D: Failure:D \failure) { failure }
    multi method POSITIONS(Block:D $self: Any:D \list) {
      nqp::if(
        (nqp::istype(
          (my \n := nqp::getattr(
            nqp::getattr($self,Code,'$!signature'),Signature,'$!count')
          ),Num) && nqp::isnanorinf(n)) || nqp::iseq_i(nqp::unbox_i(n),1),
        $self(nqp::if(nqp::isconcrete(list),list.elems,0)),
        $self(|(nqp::if(nqp::isconcrete(list),list.elems,0) xx n))
      )
    }
}

# vim: ft=perl6 expandtab sw=4
