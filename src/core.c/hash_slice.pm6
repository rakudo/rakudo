# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(Mu $, $?, Mu $?, *%) is nodal {*}

# %h<key>
multi sub postcircumfix:<{ }>( \SELF, \key ) is raw {
    SELF.AT-KEY(key);
}
multi sub postcircumfix:<{ }>(\SELF, \key, Mu \ASSIGN) is raw {
    SELF.ASSIGN-KEY(key, ASSIGN);
}
multi sub postcircumfix:<{ }>(\SELF, \key, Mu :$BIND! is raw) is raw {
    SELF.BIND-KEY(key, $BIND);
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$delete! ) is raw {
    nqp::if($delete,SELF.DELETE-KEY(key),SELF.AT-KEY(key))
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$delete!, *%other ) is raw {
    SLICE_ONE_HASH( SELF, key, 'delete', $delete, %other )
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$exists! ) is raw {
    nqp::if($exists,SELF.EXISTS-KEY(key),!SELF.EXISTS-KEY(key))
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$exists!, *%other ) is raw {
    SLICE_ONE_HASH( SELF, key, 'exists', $exists, %other )
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? (key,SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? Pair.new(key,SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? key !! ())
      !! SLICE_ONE_HASH( SELF, key, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$v!, *%other ) is raw {
    $v && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? nqp::decont(SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'v', $v, %other );
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Iterable \key ) is raw {
    nqp::iscont(key)
      ?? SELF.AT-KEY(key)
      !! nqp::iscont(SELF) && nqp::not_i(nqp::isconcrete(SELF))
        ?? key.flatmap({ SELF{$_} }).eager.list
        !! nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
             nqp::stmts(
               Rakudo::Iterator.AssociativeIterableKeys(SELF,key)
                 .push-all(my \buffer := nqp::create(IterationBuffer)),
               buffer
             )
           )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Mu \ASSIGN) is raw {
    nqp::iscont(key)
      ?? SELF.ASSIGN-KEY(key, ASSIGN)
      !! (nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
           nqp::stmts(
             Rakudo::Iterator.AssociativeIterableKeys(SELF //= {},key)
               .push-all(my \buffer := nqp::create(IterationBuffer)),
             buffer
           )
         ) = ASSIGN)
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF,Iterable \key, Bool() :$delete!,*%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'delete', $delete, %other )
        !! SLICE_MORE_HASH( SELF, key, 'delete', $delete, %other )
}
multi sub postcircumfix:<{ }>(\SELF,Iterable \key, Bool() :$exists!,*%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'exists', $exists, %other )
        !! SLICE_MORE_HASH( SELF, key, 'exists', $exists, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Bool() :$kv!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'kv', $kv, %other )
        !! SLICE_MORE_HASH( SELF, key, 'kv', $kv, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Bool() :$p!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'p', $p, %other )
        !! SLICE_MORE_HASH( SELF, key, 'p', $p, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Bool() :$k!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'k', $k, %other )
        !! SLICE_MORE_HASH( SELF, key, 'k', $k, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Bool() :$v!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'v', $v, %other )
        !! SLICE_MORE_HASH( SELF, key, 'v', $v, %other )
}

# %h{*}
multi sub postcircumfix:<{ }>( \SELF, Whatever ) is raw {
    SELF{SELF.keys.list};
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Mu \ASSIGN) is raw {
    die "Cannot assign to *, as the order of keys is non-deterministic";
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$delete!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'delete', $delete, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$exists!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'exists', $exists, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$kv!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$k!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Bool() :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_HASH( SELF, SELF.keys.list, 'v', $v, %other )
      !! SELF{SELF.keys.list};
}

# %h{}
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) is raw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$delete!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'delete', $delete, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$exists!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'exists', $exists, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$kv!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$k!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Bool() :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_HASH( SELF, SELF.keys.list, 'v', $v, %other )
      !! SELF{SELF.keys.list};
}
multi sub postcircumfix:<{ }>(Mu \SELF, *%other ) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SELF.ZEN-KEY(|%other)
      !! nqp::decont(SELF)
}

proto sub postcircumfix:<{; }>($, $, *%) is nodal {*}

sub MD-HASH-SLICE-ONE-POSITION(\SELF, \indices, \idx, int $dim, \target) {
    my int $next-dim = $dim + 1;
    if $next-dim < indices.elems {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-HASH-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Str) {
            MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY(idx), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
        elsif nqp::istype(idx, Whatever) {
            for SELF.keys {
                MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY($_), indices, indices.AT-POS($next-dim), $next-dim, target)
            }
        }
        else  {
            MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY(idx), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
    }
    else {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-HASH-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Str) {
            nqp::push(target, SELF.AT-KEY(idx))
        }
        elsif nqp::istype(idx, Whatever) {
            for SELF.keys {
                nqp::push(target, SELF.AT-KEY($_))
            }
        }
        else {
            nqp::push(target, SELF.AT-KEY(idx))
        }
    }
}

multi sub postcircumfix:<{; }>(\SELF, @indices) {
    my \target = nqp::create(IterationBuffer);
    MD-HASH-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
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

# vim: ft=perl6 expandtab sw=4
