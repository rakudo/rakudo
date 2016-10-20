my role QuantHash does Associative {
    method Int     ( --> Int)     { self.total.Int }
    method Num     ( --> Num)     { self.total.Num }
    method Numeric ( --> Numeric) { self.total.Numeric }
    method Real    ( --> Real)    { self.total.Real }

    method list() { self.pairs.cache }

    method fmt(QuantHash: Cool $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }

    multi method AT-KEY(QuantHash:U \SELF: $key) is raw {
        SELF.^name.chars == 3 # shoddy heuristic for Set/Bag/Mix
          ?? die "Cannot auto-vivify an immutable {SELF.^name}"
          !! (SELF = self.new).AT-KEY($key)
    }
}

# vim: ft=perl6 expandtab sw=4
