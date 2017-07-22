my class Mix does Mixy {
    has $!WHICH;
    has Real $!total;
    has Real $!total-positive;

#--- interface methods
    method SET-SELF(Mix:D: \elems) {
        nqp::if(
          nqp::elems(elems),
          nqp::stmts(
            nqp::bindattr(self,::?CLASS,'$!elems',elems),
            self
          ),
          mix()
        )
    }

    multi method DELETE-KEY(Mix:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
    }

#--- introspection methods
    multi method WHICH(Mix:D:)    {
        nqp::if(
          nqp::attrinited(self,Mix,'$!WHICH'),
          $!WHICH,
          $!WHICH := ObjAt.new('Mix|' ~ nqp::sha1(
            nqp::join('\0',Rakudo::Sorting.MERGESORT-str(
              Rakudo::QuantHash.BAGGY-RAW-KEY-VALUES(self)
            ))
          ))
        )
    }
    method total(Mix:D: --> Real:D) {
        nqp::if(
          nqp::attrinited(self,Mix,'$!total'),
          $!total,
          $!total := Rakudo::QuantHash.MIX-TOTAL($!elems)
        )
    }
    method total-positive(Mix:D: --> Real:D) {
        nqp::if(
          nqp::attrinited(self,Mix,'$!total-positive'),
          $!total-positive,
          $!total-positive := Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems)
        )
    }

#--- object creation methods
    multi method new(Mix:_:) {
        nqp::if(
          nqp::eqaddr(self.WHAT,Mix),
          mix(),
          nqp::create(self)
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
    multi method Mix(Mix:D:) { self }
    multi method MixHash(Mix:D) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(MixHash)
        )
    }
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::clone(self),
          mix()
        )
    }

#--- illegal methods
    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
