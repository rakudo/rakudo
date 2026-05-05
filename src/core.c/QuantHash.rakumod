my role QuantHash does Associative {

    multi method new(QuantHash:_:) { self.WHAT.SETUP }

    method keyof() { Mu }

    # Generic handling for setting up a QuantHash
    proto method SETUP(|) is implementation-detail {*}
    multi method SETUP(QuantHash:U:) {
        self.SETUP(nqp::create(Rakudo::Internals::IterationSet))
    }
    multi method SETUP(QuantHash:U: \elems) {
        nqp::create(self).SETUP(elems)
    }
    multi method SETUP(QuantHash:D: \elems) {
        nqp::p6bindattrinvres(self,::?CLASS,'$!elems',nqp::decont(elems))
    }

    # provide a proto for QuantHashes from here
    proto method STORE(|) {*}
    multi method STORE(QuantHash:D: |) {     # for immutable types
        X::Assignment::RO.new(value => self).throw
    }

    method Int     ( --> Int:D)     { self.total.Int }
    method Num     ( --> Num:D)     { self.total.Num }
    method Numeric ( --> Numeric:D) { self.total.Numeric }
    method Real    ( --> Real:D)    { self.total.Real }

    method Capture() { self.Hash.Capture }

    multi method list(QuantHash:D:) { self.pairs.cache }

    multi method fmt(QuantHash:D: Str:D $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }

    # Default autovivifying AT-KEY on mutable QuantHashes
    multi method AT-KEY(QuantHash:U \SELF: $key) is raw {
        nqp::istype(self, Set)
          || nqp::istype(self, Bag)
          || nqp::istype(self, Mix)
          ?? X::Assignment::RO.new(value => self).throw
          !! (SELF = self.new).AT-KEY($key)
    }

    # EXISTS-KEY logic is the same for all QuantHashes, but complicated
    # by the fact that the IterationSet $!elems can not be defined in
    # this role because of visibility issues.  Hence the extended getattr
    # syntax used here
    multi method EXISTS-KEY(QuantHash:D: \k --> Bool:D) {
        nqp::hllbool(
          nqp::existskey(
            nqp::getattr(self,self.WHAT,'$!elems'),
            self.WHICHIFY(k)
          )
        )
    }

    # Default ASSIGN-KEY not allowing assignments
    multi method ASSIGN-KEY(QuantHash:D: \k, \v) {
        X::Assignment::RO.new(value => self).throw;
    }
    # Default DELETE-KEY not allowing deletions
    multi method DELETE-KEY(QuantHash:D: \k) {
        X::Assignment::RO.new(value => self).throw;
    }

    multi method pairs(QuantHash:D:) { Seq.new(self.iterator) }

    proto method Setty(|) {*}
    proto method Baggy(|) {*}
    proto method Mixy (|) {*}

    method hash() { ... }
    method Hash() { ... }
    method Map()  { ... }

    # Objectifier logic for unparameterized quanthashes
    method OBJECTIFIER() is implementation-detail { -> Mu $value { $value } }

    # WHICH logic for unparameterized quanthashes
    method WHICHIFY(Mu \value) is implementation-detail { value.WHICH }
}

my role QuantHash::KeyOf[::CONSTRAINT] {
    method keyof() { CONSTRAINT }
    method is-generic { CONSTRAINT.^archetypes.generic }
    method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment) is raw {
        self.^parameterize: type-environment.instantiate(CONSTRAINT)
    }

    # Create the Callable for checking the type of a key value and
    # potentially coerce the value.  Callable is expected to throw
    # on a typecheck failure or a failure to coerce
    my &OBJECTIFIER := Rakudo::QuantHash.MAKE-OBJECTIFIER(CONSTRAINT);
    method OBJECTIFIER() is implementation-detail { &OBJECTIFIER }

    # Convert value to WHICH string to be used in the IterationSet
    method WHICHIFY(Mu \value) is implementation-detail {
        OBJECTIFIER(value).WHICH
    }
}

# vim: expandtab shiftwidth=4
