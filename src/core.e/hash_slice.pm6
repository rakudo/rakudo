# all 6.e specific sub postcircumfix {} candidates here please

proto sub postcircumfix:<{; }>($, $, *%) is nodal {*}
multi sub postcircumfix:<{; }>(\initial-SELF, @indices, :$delete) {
    my \target   = nqp::create(IterationBuffer);
    my int $dims = @indices.elems;  # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $return-list;

    sub DELETE-KEY-recursively(\SELF, \idx, int $dim --> Nil) {
        my int $next-dim = $dim + 1;
        if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
            $return-list = 1;
            DELETE-KEY-recursively(SELF, $_, $dim) for idx;
        }
        elsif $next-dim < $dims {
            if nqp::istype(idx,Whatever) {
                $return-list = 1;
                DELETE-KEY-recursively(
                  SELF.AT-KEY($_), nqp::atpos($indices,$next-dim), $next-dim
                ) for SELF.keys;  # NOTE: not reproducible!
            }
            else  {
                DELETE-KEY-recursively(
                  SELF.AT-KEY(idx), nqp::atpos($indices,$next-dim), $next-dim
                );
            }
        }
        # $next-dim == $dims, reached leaves
        elsif nqp::istype(idx,Whatever) {
            $return-list = 1;
            if $delete {
                nqp::push(target,SELF.DELETE-KEY($_)) for SELF.keys;
            }
            else {
                nqp::push(target,SELF.AT-KEY($_)) for SELF.keys;
            }
        }
        elsif $delete {
            nqp::push(target,SELF.DELETE-KEY(idx));
        }
        else {
            nqp::push(target,SELF.AT-KEY(idx));
        }
    }

    DELETE-KEY-recursively(initial-SELF, nqp::atpos($indices,0), 0);

    $return-list
      ?? target.List
      !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
}

multi sub postcircumfix:<{; }>(\initial-SELF, @indices, :$exists!) {
    my \target   = nqp::create(IterationBuffer);
    my int $dims = @indices.elems;  # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $return-list;

    sub EXISTS-KEY-recursively(\SELF, \idx, int $dim --> Nil) {
        my int $next-dim = $dim + 1;
        if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
            $return-list = 1;
            EXISTS-KEY-recursively(SELF, $_, $dim) for idx;
        }
        elsif $next-dim < $dims {
            if nqp::istype(idx,Whatever) {
                $return-list = 1;
                EXISTS-KEY-recursively(
                  SELF.AT-KEY($_), nqp::atpos($indices,$next-dim), $next-dim
                ) for SELF.keys;  # NOTE: not reproducible!
            }
            else  {
                EXISTS-KEY-recursively(
                  SELF.AT-KEY(idx), nqp::atpos($indices,$next-dim), $next-dim
                );
            }
        }
        # $next-dim == $dims, reached leaves
        elsif nqp::istype(idx,Whatever) {
            $return-list = 1;
            if $exists {
                nqp::push(target,SELF.EXISTS-KEY($_)) for SELF.keys;
            }
            else {
                nqp::push(target,!SELF.EXISTS-KEY($_)) for SELF.keys;
            }
        }
        elsif $exists {
            nqp::push(target,SELF.EXISTS-KEY(idx));
        }
        else {
            nqp::push(target,!SELF.EXISTS-KEY(idx));
        }
    }

    EXISTS-KEY-recursively(initial-SELF, nqp::atpos($indices,0), 0);

    $return-list
      ?? target.List
      !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
}

# vim: expandtab shiftwidth=4
