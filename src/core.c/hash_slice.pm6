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
    $delete ?? SELF.DELETE-KEY(key) !! SELF.AT-KEY(key)
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$delete!, *%other ) is raw {
    SLICE_ONE_HASH( SELF, key, 'delete', $delete, %other )
}
multi sub postcircumfix:<{ }>( \SELF, \key, Bool() :$exists! ) is raw {
    $exists ?? SELF.EXISTS-KEY(key) !! !SELF.EXISTS-KEY(key)
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
multi sub postcircumfix:<{ }>(\SELF, Iterable \keys, Mu \values) is raw {
    return SELF.ASSIGN-KEY(keys, values) if nqp::iscont(keys);

    my $result := nqp::create(IterationBuffer);
    my $todo   := nqp::create(IterationBuffer);
    my $keys   := keys.iterator;
    my $values := Rakudo::Iterator.TailWith(values.iterator, Nil);

    nqp::until(
      nqp::eqaddr((my \key := $keys.pull-one),IterationEnd),
      nqp::stmts(
        nqp::push($todo,key),
        nqp::push($todo,$values.pull-one)
      )
    );

    nqp::while(
      nqp::elems($todo),
      nqp::push($result,SELF.ASSIGN-KEY(nqp::shift($todo),nqp::shift($todo)))
    );

    $result.List
}
multi sub postcircumfix:<{ }>(\SELF, Iterable \key, :$BIND! is raw) is raw {
    return SELF.BIND-KEY(key, $BIND) if nqp::iscont(key);

    my $result := nqp::create(IterationBuffer);
    my $keys   := key.iterator;
    my $binds  := $BIND.iterator;
    nqp::until(
      nqp::eqaddr((my $bind := $binds.pull-one),IterationEnd)
        || nqp::eqaddr((my $key := $keys.pull-one),IterationEnd),
      nqp::push($result, SELF.BIND-KEY($key, $bind))
    );

    # fill up if ran out of values to bind?
    nqp::until(
      nqp::eqaddr(($key := $keys.pull-one),IterationEnd),
      nqp::push($result,SELF.ASSIGN-KEY($key,Nil))
    ) if nqp::eqaddr($bind,IterationEnd);

    $result.List
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

# vim: expandtab shiftwidth=4
