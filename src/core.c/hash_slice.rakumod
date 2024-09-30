# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(Mu $, Mu $?, Mu $?, *%) is nodal {*}

# %h<key>
multi sub postcircumfix:<{ }>( \SELF, Mu \key ) is default is raw {
    SELF.AT-KEY(key)
}
multi sub postcircumfix:<{ }>(\SELF, Mu \key, Mu \ASSIGN) is raw {
    SELF.ASSIGN-KEY(key, ASSIGN)
}
multi sub postcircumfix:<{ }>(\SELF, Mu \key, Mu :$BIND! is raw) is raw {
    SELF.BIND-KEY(key, $BIND)
}

# %h<key>:delete…
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$delete! ) is raw {
    $delete ?? SELF.DELETE-KEY(key) !! SELF.AT-KEY(key)
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$k!
) is raw {
    SELF.DELETE-KEY(key) if (my \existed = SELF.EXISTS-KEY(key)) && $delete;
    existed || !$k ?? key !! ()
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$p!
) is raw {
    SELF.EXISTS-KEY(key) || !$p
      ?? Pair.new(key,$delete ?? SELF.DELETE-KEY(key) !! SELF.AT-KEY(key))
      !! ()
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$kv!
) is raw {
    SELF.EXISTS-KEY(key) || !$kv
      ?? (key,$delete ?? SELF.DELETE-KEY(key) !! SELF.AT-KEY(key))
      !! ()
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$v!
) is raw {
    SELF.EXISTS-KEY(key) || !$v
      ?? ($delete ?? SELF.DELETE-KEY(key) !! SELF.AT-KEY(key))
      !! ()
}

# %h<key>:delete:exists…
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$exists!
) is raw {
    SELF.DELETE-KEY(key)
      if (my \existed = SELF.EXISTS-KEY(key).Bool) && $delete;
    nqp::hllbool(nqp::eqaddr(nqp::decont($exists),existed))
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$exists!, Bool() :$p!
) is raw {
    SELF.DELETE-KEY(key)
      if (my \existed = SELF.EXISTS-KEY(key).Bool) && $delete;
    existed || !$p
      ?? Pair.new(key,nqp::hllbool(nqp::eqaddr(nqp::decont($exists),existed)))
      !! ()
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$delete!, Bool() :$exists!, Bool() :$kv!
) is raw {
    SELF.DELETE-KEY(key)
      if (my \existed = SELF.EXISTS-KEY(key).Bool) && $delete;
    existed || !$kv
      ?? (key,nqp::hllbool(nqp::eqaddr(nqp::decont($exists),existed)))
      !! ()
}

# %h<key>:exists…
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$exists! ) is raw {
    nqp::hllbool(nqp::eqaddr(nqp::decont($exists),SELF.EXISTS-KEY(key).Bool))
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$exists!, Bool() :$p!
) is raw {
    (my \r = nqp::eqaddr(nqp::decont($exists),SELF.EXISTS-KEY(key).Bool))
      ?? ($p ?? Pair.new(key,nqp::hllbool(r)) !! r)
      !! ()
}
multi sub postcircumfix:<{ }>(
  \SELF, Mu \key, Bool() :$exists!, Bool() :$kv!
) is raw {
    (my \r = nqp::eqaddr(nqp::decont($exists),SELF.EXISTS-KEY(key).Bool))
      ?? ($kv ?? (key,nqp::hllbool(r)) !! r)
      !! ()
}

# %h<key>:x
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$k! ) is raw {
    SELF.EXISTS-KEY(key) || !$k ?? key !! ()
}
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$p! ) is raw {
    SELF.EXISTS-KEY(key) || !$p
      ?? Pair.new(key,SELF.AT-KEY(key))
      !! ($p ?? () !! SELF.AT-KEY(key))
}
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$kv! ) is raw {
    SELF.EXISTS-KEY(key) || !$kv
      ?? (key,SELF.AT-KEY(key))
      !! ($kv ?? () !! SELF.AT-KEY(key))
}
multi sub postcircumfix:<{ }>( \SELF, Mu \key, Bool() :$v! ) is raw {
    SELF.EXISTS-KEY(key) || !$v ?? nqp::decont(SELF.AT-KEY(key)) !! ()
}

# %h<key>:huh?
multi sub postcircumfix:<{ }>( \SELF, Mu \key, *%_ ) {
    my @nogo = %_<delete exists kv p k v>:delete:k;

    X::Adverb.new(
      :what<slice>,
      :source((try SELF.VAR.name) // SELF.^name),
      :unexpected(%_.keys),
      :@nogo,
    ).Failure
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Iterable:D \key ) is raw {
    nqp::iscont(key)
      ?? SELF.AT-KEY(key)
      !! nqp::iscont(SELF) && nqp::not_i(nqp::isconcrete(SELF))
        ?? key.flatmap({ SELF{$_} }).eager.list
        !! Rakudo::Iterator.AssociativeIterableKeys(SELF, key).List
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
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, :$BIND! is raw) is raw {
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
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$delete!,*%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'delete', $delete, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'delete', $delete, %_ )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$exists!, *%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'exists', $exists, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'exists', $exists, %_ )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$kv!, *%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'kv', $kv, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'kv', $kv, %_ )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$p!, *%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'p', $p, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'p', $p, %_ )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$k!, *%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'k', $k, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'k', $k, %_ )
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \key, Bool() :$v!, *%_) is raw {
    nqp::iscont(key)
      ?? SLICE_ONE_HASH(  SELF, key, 'v', $v, %_ )
      !! SLICE_MORE_HASH( SELF, key, 'v', $v, %_ )
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
