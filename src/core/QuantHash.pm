my role QuantHash does Associative {
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
          ?? die "Cannot auto-vivify an immutable {SELF.^name}"
          !! (SELF = self.new).AT-KEY($key)
    }

    multi method pairs(QuantHash:D:) { Seq.new(self.iterator) }

    method raw_keys() {
        nqp::if(
          (my $elems := self.raw_hash),
          nqp::stmts(
            (my $keys := nqp::setelems(nqp::list_s,nqp::elems($elems))),
            (my int $i = -1),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindpos_s(
                $keys,
                ($i = nqp::add_i($i,1)),
                nqp::iterkey_s(nqp::shift($iter))
              )
            ),
            $keys
          ),
          nqp::list_s
        )
    }
}

# vim: ft=perl6 expandtab sw=4
