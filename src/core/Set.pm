my class Set does Setty {
    has $!WHICH;

    multi method WHICH (Set:D:) {
        nqp::if(
          nqp::attrinited(self,Set,'$!WHICH'),
          $!WHICH,
          $!WHICH := self.^name ~ '|' ~ %!elems.keys.sort
        )
    }

    multi method kv(Setty:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
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
        }.new(%!elems))
    }
    multi method values(Setty:D:) { True xx self.total }
    multi method pairs(Setty:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
              $!iter
                ?? Pair.new(nqp::iterval(nqp::shift($!iter)),True)
                !! IterationEnd
            }
        }.new(%!elems))
    }
    multi method antipairs(Setty:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
              $!iter
                ?? Pair.new(True,nqp::iterval(nqp::shift($!iter)))
                !! IterationEnd
            }
        }.new(%!elems))
    }

    multi method grab(Set:D: $count?) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs(Set:D $count?) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Set { self }
    method SetHash { SetHash.new(self.keys) }

    multi method AT-KEY(Set:D: \k --> Bool) {
        %!elems.EXISTS-KEY(k.WHICH);
    }
    multi method ASSIGN-KEY(Set:D: \k,\v) {
        X::Assignment::RO.new(typename => self.^name).throw;
    }
    multi method DELETE-KEY(Set:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
