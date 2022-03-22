my class PhasersList is repr('VMArray') {}
my class Block { # declared in BOOTSTRAP
    # class Block is Code
    #     has Mu $!phasers;
    #     has Mu $!why;

    proto method of() {*}
    multi method of(Block:U:) { Mu }
    multi method of(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method returns(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method add_phaser(str $name, &block --> Nil) {
        $!phasers := nqp::hash unless nqp::ishash($!phasers);

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

    # Return a Callable to run any phasers for the given name on this
    # Block.  Returns Nil if there are no phasers, the only phaser if
    # there only is one, or a Callable that will call all of the phasers.
    method callable_for_phaser(str $name) {
        nqp::if(
          nqp::ishash($!phasers)
            && (my \phasers := nqp::atkey($!phasers,$name)),
          nqp::if(
            nqp::iseq_i(nqp::elems(phasers),1),
            nqp::atpos(phasers,0),
            {
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,nqp::elems(phasers)),
                  nqp::atpos(phasers,$i)(),
                  :nohandler
                );
            }
          ),
          Nil
        )
    }

    method fire_if_phasers(str $name --> Nil) {
        if nqp::ishash($!phasers) && nqp::atkey($!phasers,$name) -> \phasers {
            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,nqp::elems(phasers)),
              nqp::atpos(phasers,$i)(),
              :nohandler
            );
        }
    }

    method fire_phasers(str $name --> Nil) {
        my \phasers := nqp::atkey($!phasers,$name);
        my int $i    = -1;
        nqp::while(
          nqp::islt_i(++$i,nqp::elems(phasers)),
          nqp::atpos(phasers,$i)(),
          :nohandler
        );
    }

    method has-phasers() {
        nqp::hllbool(nqp::ishash($!phasers))
    }
    method has-loop-phasers() {
        nqp::hllbool(
          nqp::ishash($!phasers)
            && (    nqp::existskey($!phasers,'NEXT')
                 || nqp::existskey($!phasers,'LAST')
                 || nqp::existskey($!phasers,'FIRST')
               )
        )
    }

    method has-phaser(str $name) {
        nqp::hllbool(
          nqp::ishash($!phasers) && nqp::existskey($!phasers,$name)
        )
    }

    method phasers(str $name) {
        nqp::ishash($!phasers)
          && nqp::existskey($!phasers,$name)
          ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
               nqp::atkey($!phasers,$name))
          !! ()
    }

    multi method raku(Block:D:) {
        "-> {self.signature.raku.substr(2,*-1)} \{ #`({self.WHICH}) ... \}"
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
    multi method POSITIONS(Block:D $self: \list) {
        nqp::isconcrete(list)
          ?? (nqp::istype(
               (my \count := nqp::getattr(
                 nqp::getattr($self,Code,'$!signature'),Signature,'$!count'
               )),
               Num
              ) && nqp::isnanorinf(count)
             ) || nqp::iseq_i(count,1)
            ?? $self(list.elems)
            !! $self(|(list.elems xx count))
          !! $self(0)
    }
}

# vim: expandtab shiftwidth=4
