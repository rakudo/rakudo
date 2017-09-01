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
    multi method new(Set:_:) {
        nqp::if(
          nqp::eqaddr(self.WHAT,Set),
          set(),
          nqp::create(self)
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
                nqp::join('\0',Rakudo::Sorting.MERGESORT-str(
                  Rakudo::QuantHash.RAW-KEYS(self)
                ))
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
        }.new($!elems)
    }

    multi method kv(Set:D:) {
        Seq.new(class :: does Rakudo::QuantHash::Quanty-kv {
            method pull-one() is raw {
                nqp::if(
                  $!on,
                  nqp::stmts(
                    ($!on = 0),
                    True,
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on = 1),
                      nqp::iterval(nqp::shift($!iter))
                    ),
                    IterationEnd
                  )
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    $target.push(nqp::iterval(nqp::shift($!iter))),
                    $target.push(True)
                  )
                )
            }
        }.new(self))
    }
    multi method values(Set:D:) { True xx self.total }

    multi method grab(Set:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Set:D $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    multi method Set(Set:D:) { self }
    multi method SetHash(Set:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::p6bindattrinvres(
            nqp::create(SetHash),SetHash,'$!elems',nqp::clone($!elems)
          ),
          nqp::create(SetHash)
        )
    }
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::clone(self),
          set()
        )
    }

#--- interface methods
    multi method AT-KEY(Set:D: \k --> Bool:D) {
        nqp::p6bool($!elems && nqp::existskey($!elems,k.WHICH))
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) {
        X::Assignment::RO.new(value => self).throw;
    }
    multi method DELETE-KEY(Set:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
