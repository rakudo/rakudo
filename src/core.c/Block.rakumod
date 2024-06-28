my class Block { # declared in BOOTSTRAP
    # class Block is Code
    #     has Mu $!phasers;
    #     has Mu $!why;

    proto method of() {*}
    multi method of(Block:U:) { Mu }
    multi method of(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method returns(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    # These methods cannot be private methods as private method dispatch is
    # not working yet this early in the setting.
    method unshift-phaser(str $name, &block --> Nil) is implementation-detail {
        nqp::unshift(
          nqp::ifnull(
            nqp::atkey($!phasers,$name),
            nqp::bindkey($!phasers,$name,nqp::create(IterationBuffer))
          ),
          &block
        )
    }
    method push-phaser(str $name, &block --> Nil) is implementation-detail {
        nqp::push(
          nqp::ifnull(
            nqp::atkey($!phasers,$name),
            nqp::bindkey($!phasers,$name,nqp::create(IterationBuffer))
          ),
          &block
        )
    }

    proto method WhateverCode(|) {*}
    multi method WhateverCode(Block:U:) { WhateverCode }
    multi method WhateverCode(Block:D:) {
        if nqp::isconcrete($!phasers) {
            die "Cannot convert to WhateverCode because the Block has phasers";
        }
        else {
            my $wc := nqp::create(WhateverCode);
            nqp::bindattr(
              $wc,Code,'$!do',nqp::getattr(self,Code,'$!do')
            );
            nqp::bindattr(
              $wc,Code,'$!signature',nqp::getattr(self,Code,'$!signature')
            );
            nqp::bindattr(
              $wc,Code,'@!compstuff',nqp::getattr(self,Code,'@!compstuff')
            );
            $wc
        }
    }

    method fatalize() is implementation-detail {
        self.add_phaser: 'POST', -> $_ {
            nqp::istype($_,Failure) ?? .throw !! True
        }
    }

    method add_phaser(str $name, &block --> Nil) {
        # $!phasers is either a Block (which indicates the fast path for
        # handling an only LEAVE phaser), or a hash (indicating one or more
        # other phasers) or not concrete (no phasers at all).

        # adding another phaser after a lone LEAVE phaser?
        if nqp::isconcrete($!phasers) && nqp::not_i(nqp::ishash($!phasers)) {
            my &leave := $!phasers;
            $!phasers := nqp::hash;
            self.unshift-phaser('!LEAVE-ORDER', &leave);
            self.unshift-phaser('LEAVE', &leave);
        }

        # NOTE: nqp::iseq_s is needed as it is too early in the setting to
        # have eq work on native strings
        if nqp::iseq_s($name,'LEAVE') {
            if nqp::isconcrete($!phasers) {
                self.unshift-phaser('!LEAVE-ORDER', &block);  # slow leaving
                self.unshift-phaser($name, &block);           # introspection
            }
            else {
                $!phasers := &block;  # fast path for an only LEAVE phaser
            }
        }
        else {
            $!phasers := nqp::hash unless nqp::isconcrete($!phasers);

            if nqp::iseq_s($name,'KEEP') || nqp::iseq_s($name,'UNDO') {
                nqp::unshift(
                  nqp::ifnull(
                    nqp::atkey($!phasers,'!LEAVE-ORDER'),
                    nqp::bindkey(
                      $!phasers,'!LEAVE-ORDER',nqp::create(IterationBuffer))
                  ),
                  nqp::list($name,&block)
                );
                self.unshift-phaser($name, &block);
            }
            else {
                nqp::iseq_s($name,'NEXT') || nqp::iseq_s($name,'POST')
                  ?? self.unshift-phaser($name, &block)
                  !! self.push-phaser($name, &block);
            }
        }
    }

    # Return a Callable to run any phasers for the given name on this
    # Block.  Returns Nil if there are no phasers, the only phaser if
    # there only is one, or a Callable that will call all of the phasers.
    method callable_for_phaser(str $name) {
        nqp::ishash($!phasers) && (my \blocks := nqp::atkey($!phasers,$name))
          ?? nqp::iseq_i(nqp::elems(blocks),1)
            ?? nqp::atpos(blocks,0)
            !! {
                   my int $i = -1;
                   nqp::while(
                     ++$i < nqp::elems(blocks),
                     nqp::atpos(blocks,$i)(),
                     :nohandler
                   );
               }
          !! nqp::isconcrete($!phasers) && nqp::iseq_s($name,'LEAVE')
            ?? $!phasers  # lone LEAVE phaser
            !! Nil
    }

    method fire_if_phasers(str $name --> Nil) {
        if nqp::isconcrete($!phasers) {
            if nqp::ishash($!phasers)
              && nqp::atkey($!phasers,$name) -> \blocks {
                my int $i = -1;
                nqp::while(
                  ++$i < nqp::elems(blocks),
                  nqp::atpos(blocks,$i)(),
                  :nohandler
                );
            }
            elsif nqp::iseq_s($name,'LEAVE') {
                $!phasers();  # lone LEAVE phaser
            }
        }
    }

    method fire_phasers(str $name --> Nil) {
        if nqp::ishash($!phasers) {
            if nqp::atkey($!phasers,$name) -> \blocks {
                my int $i   = -1;
                nqp::while(
                  ++$i < nqp::elems(blocks),
                  nqp::atpos(blocks,$i)(),
                  :nohandler
                );
            }
        }
        elsif nqp::isconcrete($!phasers) && nqp::iseq_s($name,'LEAVE') {
            $!phasers();
        }
    }

    method has-phasers() {
        nqp::hllbool(nqp::isconcrete($!phasers))
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
          (nqp::ishash($!phasers) && nqp::existskey($!phasers,$name))
            || (nqp::iseq_s($name,'LEAVE') && nqp::isconcrete($!phasers))
        )
    }

    method phasers(str $name) {
        nqp::ishash($!phasers)
          ?? nqp::existskey($!phasers,$name)
            ?? nqp::atkey($!phasers,$name).List
            !! ()
          !! nqp::iseq_s($name,'LEAVE') && nqp::isconcrete($!phasers)
            ?? ($!phasers,)
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
