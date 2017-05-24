my class Mix does Mixy {
    has $!WHICH;
    has Real $!total;

#--- interface methods
    method SET-SELF(Mix:D: \elems) {
        nqp::if(
          nqp::elems(elems),
          nqp::stmts(                 # need to have allocated %!elems
            nqp::bindattr(%!elems,Map,'$!storage',elems),
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
          $!WHICH := self!WHICH
        )
    }
    method total(Mix:D: --> Real:D) {
        nqp::if(
          nqp::attrinited(self,Mix,'$!total'),
          $!total,
          $!total := Rakudo::QuantHash.MIX-TOTAL(self.raw_hash)
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

    method clone() { nqp::clone(self) }

    proto method classify-list(|) {
        X::Immutable.new(:method<classify-list>, :typename(self.^name)).throw;
    }
    proto method categorize-list(|) {
        X::Immutable.new(:method<categorize-list>, :typename(self.^name)).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
