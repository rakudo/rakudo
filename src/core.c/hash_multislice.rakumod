# all sub postcircumfix {;} candidates here please

proto sub postcircumfix:<{; }>($, $, Mu $?, *%) is nodal {*}

my class X::Bind::Slice { ... }
my class X::Bind::ZenSlice { ... }
my class X::NotEnoughDimensions { ... }
my class X::TooManyDimensions { ... }

# The keys of a multidimensional subscript when each dimension takes one
# key, or Nil when any of them takes a slice.
sub MD-SINGLE-KEYS(@indices) is raw is implementation-detail {
    my int $elems = @indices.elems;  # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my \keys    := nqp::create(IterationBuffer);
    my int $i = -1;
    nqp::while(
      nqp::islt_i(++$i,$elems),
      nqp::if(
        nqp::istype((my \index := nqp::atpos(indices,$i)),Whatever)
          || nqp::istype(index,HyperWhatever)
          || (nqp::istype(index,Iterable)
               && nqp::isconcrete(index)
               && nqp::not_i(nqp::iscont(index))),
        (return Nil),
        nqp::push(keys,nqp::decont(index))
      )
    );
    nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',keys)
}

# The values an iterable gives, rather than the containers holding them
sub MD-VALUES(\iterable) is implementation-detail {
    my \values   := nqp::create(IterationBuffer);
    my \iterator := iterable.iterator;
    nqp::until(
      nqp::eqaddr((my \value := iterator.pull-one),IterationEnd),
      nqp::push(values,nqp::decont(value))
    );
    nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',values)
}

# Binds the element of the innermost hash a key per dimension reaches,
# vivifying the hashes before it as a subscript of each key in turn does.
sub MD-HASH-BIND(\target, \keys, int $i, Mu \value) is raw is implementation-detail {
    my \parts := nqp::getattr(keys,List,'$!reified');
    nqp::islt_i($i,nqp::sub_i(nqp::elems(parts),1))
      ?? MD-HASH-BIND(target.AT-KEY(nqp::atpos(parts,$i)),keys,nqp::add_i($i,1),value)
      !! target.BIND-KEY(nqp::atpos(parts,$i),value)
}

# The lists of keys a slice of a shaped hash takes, each itemized to be one
# key. A dimension taking * takes the keys of the entries whose other keys
# match, in no set order, so such a slice is not assigned or bound.
sub SHAPED-SLICE-KEYS(\SELF, @indices, str $operation) is implementation-detail {
    my int $elems = @indices.elems;  # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my \choices := nqp::list;
    my int $any;
    my int $i = -1;
    nqp::while(
      nqp::islt_i(++$i,$elems),
      nqp::if(
        nqp::istype((my \index := nqp::atpos(indices,$i)),Whatever),
        nqp::stmts(
          nqp::push(choices,Whatever),
          ($any = 1)
        ),
        nqp::if(
          nqp::istype(index,HyperWhatever),
          die("Only the last dimension of a subscript can take **"),
        nqp::push(choices,
          nqp::istype(index,Iterable)
            && nqp::isconcrete(index)
            && nqp::not_i(nqp::iscont(index))
            ?? MD-VALUES(index)
            !! nqp::p6bindattrinvres(
                 nqp::create(List),List,'$!reified',nqp::list(nqp::decont(index))
               )
        ))
      )
    );

    if $any {
        my int $dims = SELF.SHAPED-DIMENSIONS;
        my str $doing = $operation eq 'assign'
          ?? 'assign to'
          !! $operation eq 'bind' ?? 'bind to' !! 'access';
        X::NotEnoughDimensions.new(
          :operation($doing), :aggregate<hash>,
          :got-dimensions($elems), :needed-dimensions($dims)
        ).throw if $elems < $dims;
        X::TooManyDimensions.new(
          :operation($doing), :aggregate<hash>,
          :got-dimensions($elems), :needed-dimensions($dims)
        ).throw if $elems > $dims;
        die "Cannot assign to *, as the order of keys is non-deterministic"
          if $operation eq 'assign';
        X::Bind::Slice.new(type => SELF.WHAT).throw
          if $operation eq 'bind';

        # the WHICH of each key given for a dimension, as the hash takes it
        my \whiches := nqp::list;
        $i = -1;
        while ++$i < $elems {
            my \choice := nqp::atpos(choices,$i);
            if nqp::eqaddr(choice,Whatever) {
                nqp::push(whiches,Whatever);
            }
            else {
                my \seen := nqp::hash;
                nqp::bindkey(seen,SELF.SHAPED-KEY-PART($i,$_).WHICH.Str,True)
                  for choice;
                nqp::push(whiches,seen);
            }
        }
        SELF.keys.grep(-> \key {
            my int $match = 1;
            my int $j = -1;
            nqp::while(
              $match && nqp::islt_i(++$j,$elems),
              nqp::unless(
                nqp::eqaddr((my \seen := nqp::atpos(whiches,$j)),Whatever),
                ($match = nqp::existskey(seen,key.AT-POS($j).WHICH.Str))
              )
            );
            $match
        }).map({ .item }).List.eager
    }
    else {
        cross(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',choices))
          .map({ .List.item }).List.eager
    }
}

# A slice of a shaped hash for the lists of keys it takes. A junction index
# its dimension does not take gives a junction of the slice for each of its
# eigenstates.
sub SHAPED-SLICE(\SELF, @indices, &slice, str $operation) is raw is implementation-detail {
    my int $elems = @indices.elems;  # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my \types   := nqp::getattr(SELF.shape.eager,List,'$!reified');
    my int $i = -1;
    while ++$i < $elems {
        my \index := nqp::decont(nqp::atpos(indices,$i));
        return index.THREAD(-> \eigenstate {
            my \threaded := nqp::clone(indices);
            nqp::bindpos(threaded,$i,eigenstate);
            SHAPED-SLICE(
              SELF,
              nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',threaded),
              &slice,
              $operation
            )
        }) if nqp::istype(index,Junction)
          && $i < nqp::elems(types)
          && nqp::not_i(nqp::istype(index,nqp::atpos(types,$i)));
    }
    slice(SHAPED-SLICE-KEYS(SELF, @indices, $operation))
}

# The indices of a multidimensional subscript of a shaped hash, with a
# trailing ** taking * for any number of dimensions left out, as does each
# of them when reading
sub SHAPED-WILDCARD(\SELF, @indices, int $reading) is implementation-detail {
    my int $elems = @indices.elems;  # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $hyper = $elems && nqp::istype(
      nqp::decont(nqp::atpos(indices,nqp::sub_i($elems,1))),HyperWhatever
    );
    my int $given = nqp::sub_i($elems,$hyper);
    my int $dims  = SELF.SHAPED-DIMENSIONS;
    if $hyper || ($reading && $given < $dims) {
        my \wildcarded := nqp::create(IterationBuffer);
        my int $i = -1;
        nqp::push(wildcarded,nqp::atpos(indices,$i)) while ++$i < $given;
        nqp::push(wildcarded,Whatever.new) while $i++ < $dims;
        wildcarded.List
    }
    else {
        @indices
    }
}

# Whether each of a slice of keys of a shaped hash is a list of keys
sub SHAPED-KEY-LISTS(\keys) is implementation-detail {
    my \iterator := keys.cache.iterator;
    nqp::until(
      nqp::eqaddr((my \key := iterator.pull-one),IterationEnd),
      nqp::unless(
        nqp::istype(nqp::decont(key),List) || nqp::istype(nqp::decont(key),Seq),
        (return False)
      )
    );
    True
}

# The indices of the multidimensional subscript an iterable subscript of a
# shaped hash stands for, or Nil for a list of a key for each dimension or a
# slice of such lists, which the hash takes as an object hash does
sub SHAPED-ONE-DIMENSION(\SELF, \key) is implementation-detail {
    nqp::iscont(key)
      ?? nqp::istype(key,List) || nqp::istype(key,Seq)
        ?? key.is-lazy
             || (my \keys := key.cache).elems >= SELF.SHAPED-DIMENSIONS
          ?? Nil
          !! keys
        !! (key,)
      !! SHAPED-KEY-LISTS(key)
        ?? Nil
        !! (key,)
}

# Whether any index of a subscript takes any key of its dimension, so the
# order of the elements it gives is not set
sub SHAPED-ANY-KEY(\indices) is implementation-detail {
    my \iterator := indices.iterator;
    nqp::until(
      nqp::eqaddr((my \index := iterator.pull-one),IterationEnd),
      nqp::if(nqp::istype(nqp::decont(index),Whatever),(return True))
    );
    False
}

# A shaped hash gives the element itself for one key per dimension, as the
# subscript of one key does, and a list of elements for a slice. Any adverb
# goes to that subscript with each list of keys as one key.
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, Mu \assignee) is raw {
    my \indices := SHAPED-WILDCARD(SELF, @indices, 0);
    nqp::isconcrete(my \key := MD-SINGLE-KEYS(indices))
      ?? SELF.ASSIGN-KEY(key, assignee)
      !! SHAPED-SLICE(SELF, indices,
           -> \keys { &postcircumfix:<{ }>(SELF, keys, assignee) }, 'assign')
}
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, Mu :$BIND! is raw) is raw {
    my \indices := SHAPED-WILDCARD(SELF, @indices, 0);
    nqp::isconcrete(my \key := MD-SINGLE-KEYS(indices))
      ?? SELF.BIND-KEY(key, $BIND)
      !! SHAPED-SLICE(SELF, indices,
           -> \keys { &postcircumfix:<{ }>(SELF, keys, :$BIND) }, 'bind')
}
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, *%adverbs) is raw {
    my \indices := SHAPED-WILDCARD(SELF, @indices, 1);
    my \key     := MD-SINGLE-KEYS(indices);
    nqp::elems(nqp::getattr(%adverbs,Map,'$!storage'))
      ?? nqp::isconcrete(key)
        ?? &postcircumfix:<{ }>(SELF, key.item, |%adverbs)
        !! SHAPED-SLICE(SELF, indices,
             -> \keys { &postcircumfix:<{ }>(SELF, keys, |%adverbs) }, '')
      !! nqp::isconcrete(key)
        ?? SELF.AT-KEY(key)
        !! SHAPED-ANY-KEY(indices)
          ?? SHAPED-SLICE(SELF, indices,
               -> \keys { MD-VALUES(&postcircumfix:<{ }>(SELF, keys)) }, '')
          !! SHAPED-SLICE(SELF, indices,
               -> \keys { &postcircumfix:<{ }>(SELF, keys) }, '')
}

# A shaped hash takes a subscript of one dimension as a multidimensional one,
# so any dimension left out takes any key. A slice or ** that another of
# these candidates passes on to the subscript of any hash is passed on again.
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, Mu \key, *%adverbs) is raw {
    nqp::istype(key,HyperWhatever)
      || (nqp::istype(key,Iterable) && nqp::isconcrete(key))
      ?? nextsame()
      !! &postcircumfix:<{; }>(SELF, (key,), |%adverbs)
}
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, Whatever, *%adverbs) is raw {
    &postcircumfix:<{; }>(SELF, (nqp::create(HyperWhatever),), |%adverbs)
}
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, HyperWhatever \key, *%adverbs) is raw {
    my \adverbs := nqp::getattr(%adverbs,Map,'$!storage');
    nqp::existskey(adverbs,'deepk')
      || nqp::existskey(adverbs,'deepkv')
      || nqp::existskey(adverbs,'tree')
      ?? nextsame()
      !! &postcircumfix:<{; }>(SELF, (key,), |%adverbs)
}

# A zen slice with an adverb takes every key as %h{*} does, so each key
# stays the list of a key for each dimension
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, *%adverbs) is raw {
    nqp::elems(my \adverbs := nqp::getattr(%adverbs,Map,'$!storage'))
      ?? nqp::existskey(adverbs,'BIND')
        ?? X::Bind::ZenSlice.new(type => SELF.WHAT).throw
        !! &postcircumfix:<{; }>(SELF, (nqp::create(HyperWhatever),), |%adverbs)
      !! nqp::decont(SELF)
}

# A list of a key for each dimension, or a slice of such lists, is taken as
# an object hash takes a key. Any other list is a list of keys for fewer
# dimensions or a slice of the first.
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, Iterable:D \key, *%adverbs) is raw {
    nqp::isconcrete(my \indices := SHAPED-ONE-DIMENSION(SELF, key))
      ?? &postcircumfix:<{; }>(SELF, indices, |%adverbs)
      !! nqp::iscont(key)
           && nqp::not_i(nqp::elems(nqp::getattr(%adverbs,Map,'$!storage')))
        ?? SELF.AT-KEY(key)
        !! nextsame
}

# A junction of keys gives a junction of what each of them gives, unless
# the first dimension takes the junction itself as a key
multi sub postcircumfix:<{ }>(Hash::Shaped \SELF, Junction \key, *%adverbs) is raw {
    nqp::istype(key,SELF.shape.head)
      ?? &postcircumfix:<{; }>(SELF, (key,), |%adverbs)
      !! key.THREAD(-> \eigenstate {
             &postcircumfix:<{ }>(SELF, eigenstate.item, |%adverbs)
         })
}

# Binding through a multidimensional subscript of a hash binds the element
# of its innermost hash.
multi sub postcircumfix:<{; }>(\SELF, @indices, Mu :$BIND! is raw) is raw {
    nqp::isconcrete(my \keys := MD-SINGLE-KEYS(@indices))
      ?? @indices.elems
        ?? MD-HASH-BIND(SELF, keys, 0, $BIND)
        !! X::Bind::ZenSlice.new(type => SELF.WHAT).throw
      !! X::Bind::Slice.new(type => SELF.WHAT).throw
}

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

multi sub postcircumfix:<{; }>(\SELF, @indices, Mu \assignee) is raw {
    my \target := &postcircumfix:<{; }>(SELF, @indices);
    target = assignee
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
