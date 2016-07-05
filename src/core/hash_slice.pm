# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(|) is nodal { * }

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
multi sub postcircumfix:<{ }>( \SELF, \key, :$SINK!, *%other ) is raw {
    SLICE_ONE_HASH( SELF, key, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$delete!, *%other ) is raw {
    nqp::if(
      $delete && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage'))),
      SELF.DELETE-KEY(key),
      SLICE_ONE_HASH( SELF, key, 'delete', $delete, %other )
    )
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$exists!, *%other ) is raw {
    nqp::if(
      $exists && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage'))),
      SELF.EXISTS-KEY(key),
      SLICE_ONE_HASH( SELF, key, 'exists', $exists, %other )
    )
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? (key,SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? Pair.new(key,SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? key !! ())
      !! SLICE_ONE_HASH( SELF, key, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$v!, *%other ) is raw {
    $v && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-KEY(key) ?? nqp::decont(SELF.AT-KEY(key)) !! ())
      !! SLICE_ONE_HASH( SELF, key, 'v', $v, %other );
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Iterable \key ) is raw {
    nqp::iscont(key)
      ?? SELF.AT-KEY(key)
      !! key.flatmap({ SELF{$_} }).eager.list;
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, Mu \ASSIGN) is raw {
    nqp::iscont(key)
      ?? SELF.ASSIGN-KEY(key, ASSIGN)
      !! (key.flatmap({ SELF{$_} }).eager.list = ASSIGN)
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF,Iterable \key, :$SINK!,*%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'SINK', $SINK, %other )
        !! SLICE_MORE_HASH( SELF, key, 'SINK', $SINK, %other )
}
multi sub postcircumfix:<{ }>(\SELF,Iterable \key, :$delete!,*%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'delete', $delete, %other )
        !! SLICE_MORE_HASH( SELF, key, 'delete', $delete, %other )
}
multi sub postcircumfix:<{ }>(\SELF,Iterable \key, :$exists!,*%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'exists', $exists, %other )
        !! SLICE_MORE_HASH( SELF, key, 'exists', $exists, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$kv!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'kv', $kv, %other )
        !! SLICE_MORE_HASH( SELF, key, 'kv', $kv, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$p!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'p', $p, %other )
        !! SLICE_MORE_HASH( SELF, key, 'p', $p, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$k!, *%other) is raw {
    nqp::iscont(key)
        ?? SLICE_ONE_HASH(  SELF, key, 'k', $k, %other )
        !! SLICE_MORE_HASH( SELF, key, 'k', $k, %other )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$v!, *%other) is raw {
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
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$SINK!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$delete!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'delete', $delete, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$exists!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'exists', $exists, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$kv!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$k!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_HASH( SELF, SELF.keys.list, 'v', $v, %other )
      !! SELF{SELF.keys.list};
}

# %h{}
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) is raw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, :$SINK!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$delete!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'delete', $delete, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$exists!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'exists', $exists, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$kv!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'kv', $kv, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$k!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'k', $k, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is raw {
    SLICE_MORE_HASH( SELF, SELF.keys.list, 'p', $p, %other );
}
multi sub postcircumfix:<{ }>(\SELF, :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_HASH( SELF, SELF.keys.list, 'v', $v, %other )
      !! SELF{SELF.keys.list};
}
multi sub postcircumfix:<{ }>( \SELF, *%other ) is raw {
    SELF.ZEN-KEY(|%other);
}


proto sub postcircumfix:<{; }>(|) is nodal { * }

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
    my \target = IterationBuffer.new;
    MD-HASH-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', target)
}

# vim: ft=perl6 expandtab sw=4
