my role QuantHash does Associative {

    method keyof() { Mu }
    method item() is raw { my $ = self }

    method SET-SELF(QuantHash:D: \elems) is implementation-detail {
        nqp::bindattr(self,::?CLASS,'$!elems',elems)
          if nqp::elems(elems);
        self
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

    multi method list(QuantHash:U:) { self.Any::list }
    multi method list(QuantHash:D:) { self.pairs.cache }

    method fmt(QuantHash: Cool $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }

    multi method AT-KEY(QuantHash:U \SELF: $key) is raw {
        nqp::istype(self, Set) || nqp::istype(self, Bag) || nqp::istype(self, Mix)
          ?? X::AdHoc.new(payload => "Cannot auto-vivify an immutable {SELF.^name}").throw
          !! (SELF = self.new).AT-KEY($key)
    }

    multi method pairs(QuantHash:D:) { Seq.new(self.iterator) }

    proto method Setty(|) {*}
    proto method Baggy(|) {*}
    proto method Mixy (|) {*}

    method hash() { ... }
    method Hash() { ... }
    method Map()  { ... }
}

# vim: expandtab shiftwidth=4
