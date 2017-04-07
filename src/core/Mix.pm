my class Mix does Mixy {
    has $!WHICH;
    has Real $!total;

#--- interface methods
    multi method DELETE-KEY(Mix:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- introspection methods
    multi method WHICH(Mix:D:)    {
        nqp::if(
          nqp::attrinited(self,Mix,'$!WHICH'),
          $!WHICH,
          $!WHICH := self!WHICH
        )
    }
    method total(Mix:D: --> Real:D) {
        nqp::if(
          nqp::attrinited(self,Mix,'$!total'),
          $!total,
          $!total := self!TOTAL
        )
    }

#--- selection methods
    multi method grab($count? --> Real:D) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real:D) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

#--- coercion methods
    method Mix() { self }
    method MixHash() {
        nqp::if(
          (my $raw := nqp::getattr(%!elems,Map,'$!storage'))
            && nqp::elems($raw),
          nqp::stmts(                             # something to coerce
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s(my $tmp := nqp::shift($iter)),
                nqp::p6bindattrinvres(
                  nqp::clone(nqp::iterval($tmp)),
                  Pair,
                  '$!value',
                  (nqp::p6scalarfromdesc(nqp::null) =
                    nqp::getattr(nqp::iterval($tmp),Pair,'$!value'))
                )
              )
            ),
            nqp::create(MixHash).SET-SELF($elems)
          ),
          nqp::create(MixHash)                    # nothing to coerce
        )
    }

    method !BAGGIFY(\type, int $bind) {
        nqp::if(
          (my $raw := nqp::getattr(%!elems,Map,'$!storage'))
            && nqp::elems($raw),
          nqp::stmts(                             # something to coerce
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::isgt_i(
                  (my $value := nqp::getattr(
                  nqp::iterval(my $tmp := nqp::shift($iter)),
                  Pair,
                  '$!value'
                  ).Int),
                  0
                ),
                nqp::bindkey(                     # ok to keep value.Int
                  $elems,
                  nqp::iterkey_s($tmp),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($tmp)),
                    Pair,
                    '$!value',
                    nqp::if(
                      $bind,
                      $value,
                      (nqp::p6scalarfromdesc(nqp::null) = $value)
                    )
                  )
                ),
                nqp::deletekey(                   # we don't do <= 0 in bags
                  $elems,
                  nqp::iterkey_s($tmp)
                )
              )
            ),
            nqp::if(
              nqp::elems($elems),
              nqp::create(type).SET-SELF($elems),
              nqp::if(
                nqp::istype(type,Bag),
                bag(),
                nqp::create(type)                 # nothing left
              )
            )
          ),
          nqp::if(
            nqp::istype(type,Bag),
            bag(),
            nqp::create(type)                     # nothing to coerce
          )
        )
    }

    method Bag()     { self!BAGGIFY(Bag,     1) }
    method BagHash() { self!BAGGIFY(BagHash, 0) }

    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
