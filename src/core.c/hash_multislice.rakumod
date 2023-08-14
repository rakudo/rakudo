# all sub postcircumfix {;} candidates here please

proto sub postcircumfix:<{; }>($, $, *%) is nodal {*}

multi sub postcircumfix:<{; }>(\SELF, @indices) {
    my \target   = nqp::create(IterationBuffer);
    my int $dims = @indices.elems;  # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');

    sub MD-HASH-SLICE-ONE-POSITION(\SELF, \idx, int $dim --> Nil) {
        my int $next-dim = $dim + 1;
        if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
            MD-HASH-SLICE-ONE-POSITION(SELF, $_, $dim)
              for idx;
        }
        elsif $next-dim < $dims {
            if nqp::istype(idx,Whatever) {
                MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY($_),
                  nqp::atpos($indices,$next-dim), $next-dim)
                  for SELF.keys;
            }
            else  {
                MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY(idx),
                  nqp::atpos($indices,$next-dim), $next-dim);
            }
        }
        # $next-dim == $dims
        elsif nqp::istype(idx,Whatever) {
            nqp::push(target, SELF.AT-KEY($_)) for SELF.keys;
        }
        else {
            nqp::push(target, SELF.AT-KEY(idx));
        }
    }

    MD-HASH-SLICE-ONE-POSITION(SELF, nqp::atpos($indices,0), 0);
    target.List
}

multi sub postcircumfix:<{; }>(\SELF, @indices, :$exists!) {
    sub recurse-at-key(\SELF, \indices) {
        my \idx     := indices[0];
        my \exists  := SELF.EXISTS-KEY(idx);
        nqp::if(
            nqp::istype(idx, Iterable),
            idx.map({ |recurse-at-key(SELF, ($_, |indices.skip.cache)) }).List,
            nqp::if(
                nqp::iseq_I(indices.elems, 1),
                exists,
                nqp::if(
                    exists,
                    recurse-at-key(SELF{idx}, indices.skip.cache),
                    nqp::stmts(
                        (my \times := indices.map({ .elems }).reduce(&[*])),
                        nqp::if(
                            nqp::iseq_I(times, 1),
                            False,
                            (False xx times).List
                        )
                    ).head
                )
            )
        );
    }

    recurse-at-key(SELF, @indices)
}

# vim: expandtab shiftwidth=4
