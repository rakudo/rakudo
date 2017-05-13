my class Set does Setty {
    has $!WHICH;

    method SET-SELF(\elems) {
        nqp::if(
          nqp::elems(elems),
          nqp::stmts(
            nqp::bindattr(self,::?CLASS,'$!elems',elems),
            self
          ),
          set()
        )
    }

    multi method WHICH (Set:D:) {
        nqp::if(
          nqp::attrinited(self,Set,'$!WHICH'),
          $!WHICH,
          $!WHICH := nqp::if(
            nqp::istype(self.WHAT,Set),
            'Set|',
            nqp::concat(self.^name,'|')
          ) ~ nqp::sha1(
               nqp::join('\0',Rakudo::Sorting.MERGESORT-str(self.raw_keys))
            )
        )
    }

    method iterator(Set:D:) {
        class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
              nqp::if(
                $!iter,
                Pair.new(nqp::iterval(nqp::shift($!iter)),True),
                IterationEnd
              )
            }
        }.new(self.hll_hash)
    }

    multi method kv(Set:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            has int $!on-value;
            method pull-one() is raw {
                nqp::if(
                  $!on-value,
                  nqp::stmts(
                    ($!on-value = 0),
                    True,
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on-value = 1),
                      nqp::iterval(nqp::shift($!iter))
                    ),
                    IterationEnd
                  )
                )
            }
            method skip-one() {
                nqp::if(
                  $!on-value,
                  nqp::not_i($!on-value = 0),   # skipped a value
                  nqp::if(
                    $!iter,                     # if false, we didn't skip
                    nqp::stmts(                 # skipped a key
                      nqp::shift($!iter),
                      ($!on-value = 1)
                    )
                  )
                )
            }
            method count-only() {
                nqp::p6box_i(
                  nqp::add_i(nqp::elems($!storage),nqp::elems($!storage))
                )
            }
        }.new(self.hll_hash))
    }
    multi method values(Set:D:) { True xx self.total }

    multi method grab(Set:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Set:D $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    multi method Set(Set:D:) { self }
    multi method SetHash(Set:D:) {
        nqp::if(
          $!elems,
          nqp::p6bindattrinvres(
            nqp::create(SetHash),SetHash,'$!elems',$!elems.clone
          ),
          nqp::create(SetHash)
        )
    }

    method clone() { nqp::clone(self) }

    multi method AT-KEY(Set:D: \k --> Bool:D) {
        nqp::p6bool($!elems && nqp::existskey($!elems,k.WHICH))
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) {
        X::Assignment::RO.new(typename => self.^name).throw;
    }
    multi method DELETE-KEY(Set:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
