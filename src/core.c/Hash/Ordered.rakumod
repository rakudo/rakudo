my class Hash::Ordered is Hash {
    has str @.keys;

    method STORE(Hash::Ordered:D: |c) {
        my str @*KEYS;
        self.Hash::STORE(|c);
        @!keys := @*KEYS;
        self
    }

    method STORE_AT_KEY(Hash::Ordered:D: Str() $key, Mu \value --> Nil) {
        nqp::push_s(@*KEYS,$key);
        self.Hash::STORE_AT_KEY($key, value);
    }

    method AT-KEY(Hash::Ordered:D: Str() $key) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::stmts(
            nqp::push_s(@!keys,$key),
            nextsame;
          )
        )
    }

    method ASSIGN-KEY(Hash::Ordered:D: Str() $key, Mu \value) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::stmts(
            nqp::push_s(@!keys,$key),
            self.Hash::AT-KEY($key)
          )
        ) = value
    }

    method BIND-KEY(Hash::Ordered:D: Str() $key, Mu \value) is raw {
        nqp::push_s(@!keys,$key)
          unless nqp::existskey(nqp::getattr(self,Map,'$!storage'),$key);

        self.Hash::BIND-KEY($key, value)
    }

    method keys(Hash::Ordered:D:) {
        my $storage := nqp::getattr(self,Map,'$!storage');
        my $old     := @!keys;

        # all ok
        if nqp::elems(@!keys) == nqp::elems($storage) {
            $old
        }

        # Something changed, revisit keys
        else {
            my str @new;
            my $seen    := nqp::hash;

            my int $elems = nqp::elems($old);
            my int $i     = -1;

            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::if(
                nqp::existskey($storage,my str $key = nqp::atpos_s($old,$i))
                  && nqp::not_i(nqp::existskey($seen,$key)),
                nqp::stmts(
                  nqp::push_s(@new,$key),
                  nqp::bindkey($seen,$key,1)
                )
              )
            );

            @!keys := @new
        }
    }


    method values(Hash::Ordered:D:) { self{self.keys}    }
    method pairs( Hash::Ordered:D:) { self{self.keys}:p  }
    method kv(    Hash::Ordered:D:) { self{self.keys}:kv }

    method iterator(Hash::Ordered:D:) {
        self.pairs.iterator
    }
    method antipairs(Hash::Ordered:D:) {
        self.pairs.map(*.antipair)
    }

    method clone(Hash::Ordered:D:) {
        nqp::p6bindattrinvres(
          self.Hash::clone,Hash::Ordered,'@!keys',self.keys
        )
    }

    method raku(Hash::Ordered:D:) {
        self.^name
          ~ '.new(('
          ~ self.Hash::raku.substr(1, *-1)
          ~ '))'
    }
}

# vim: expandtab shiftwidth=4
