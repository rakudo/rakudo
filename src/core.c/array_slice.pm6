# all sub postcircumfix [] candidates here please

# Generates list of positions to index into the array at. Takes all those
# before something lazy is encountered and eagerly reifies them. If there
# are any lazy things in the slice, then we lazily consider those, but will
# truncate at the first one that is out of range. The optional
# :$eagerize will be called if Whatever/WhateverCode is encountered or if
# clipping of lazy indices is enacted.  It should return the number of
# elements of the array if called with Whatever, or do something EXISTS-POSish
# if called with an Int.  Before it does so, it may cause the calling code
# to switch to a memoized version of an iterator by modifying variables in
# the caller's scope.
proto sub POSITIONS(|) is implementation-detail {*}
multi sub POSITIONS(
  \SELF,
  \pos,
  Callable :$eagerize = -> \idx {
      nqp::istype(idx,Whatever)
        ?? nqp::isconcrete(SELF) && SELF.elems  # 0 if not concrete
        !! SELF.EXISTS-POS(idx)
  }
) {
    my class IndicesReificationTarget {
        has $!target;
        has $!star;

        method new(\target, \star) {
            my \rt = nqp::create(self);
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!star', star);
            rt
        }

        method push(Mu \value) {
            nqp::if(
              nqp::istype(value,Callable),
              nqp::stmts(
                nqp::if(
                  nqp::istype($!star,Callable),
                  nqp::bindattr(self,IndicesReificationTarget,'$!star',$!star(*))
                ),
                # just using value(...) causes stage optimize to die
                (my &whatever := value),
                nqp::if(
                  &whatever.count == Inf,
                  nqp::push($!target, whatever(+$!star)),
                  nqp::push($!target, whatever(|(+$!star xx &whatever.count)))
                )
              ),
              nqp::push($!target,value)
            )
        }
    }

    # we can optimize `42..*` Ranges; as long as they're from core, unmodified
    my \is-pos-lazy = pos.is-lazy;
    my \pos-iter    = nqp::eqaddr(pos.WHAT,Range)
        && pos.max === Inf
        && nqp::isfalse(SELF.is-lazy)
          ?? Range.new(pos.min, SELF.elems,
              :excludes-min(pos.excludes-min),
              :excludes-max(pos.excludes-max)
          ).iterator
          !! pos.iterator;

    my \pos-list = nqp::create(List);
    my \eager-indices = nqp::create(IterationBuffer);
    my \target = IndicesReificationTarget.new(eager-indices, $eagerize);
    nqp::bindattr(pos-list, List, '$!reified', eager-indices);

    if is-pos-lazy {
        # With lazy indices, we truncate at the first one that fails to exists.
        my \rest-seq = Seq.new(pos-iter).flatmap: -> Int() $i {
            nqp::unless(
              $eagerize($i),
              last,
              $i
            )
        };
        my \todo := nqp::create(List::Reifier);
        nqp::bindattr(todo, List::Reifier, '$!reified', eager-indices);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', rest-seq.iterator);
        nqp::bindattr(todo, List::Reifier, '$!reification-target', eager-indices);
        nqp::bindattr(pos-list, List, '$!todo', todo);
    }
    else {
        pos-iter.push-all: target;
    }
    pos-list
}

proto sub postcircumfix:<[ ]>($, |) is nodal {*}

multi sub postcircumfix:<[ ]>( \SELF, Any:U $type, |c ) {
    die "Unable to call postcircumfix {try SELF.VAR.name}[ $type.gist() ] with a type object\n"
      ~ "Indexing requires a defined object";
}

multi sub postcircumfix:<[ ]>(Failure:D \SELF, Any:D \pos, *%_) { SELF }

# @a[Int 1]
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos) is raw {
    SELF.AT-POS(pos)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, Mu \assignee) is raw {
    SELF.ASSIGN-POS(pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, Mu :$BIND! is raw) is raw {
    SELF.BIND-POS(pos, $BIND);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$delete!) is raw {
    $delete ?? SELF.DELETE-POS(pos) !! SELF.AT-POS(pos)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$delete!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'delete', $delete)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$exists!) is raw {
    $exists ?? SELF.EXISTS-POS(pos) !! !SELF.EXISTS-POS(pos)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$exists!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'exists', $exists)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$kv!) is raw {
    $kv
      ?? (SELF.EXISTS-POS(pos) ?? (pos, SELF.AT-POS(pos)) !! ())
      !! (pos, SELF.AT-POS(pos))
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$kv!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'kv', $kv)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$p!) is raw {
    $p
      ?? (SELF.EXISTS-POS(pos) ?? Pair.new(pos, SELF.AT-POS(pos)) !! ())
      !! Pair.new(pos, SELF.AT-POS(pos))
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$p!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'p', $p)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$k!) is raw {
    $k ?? (SELF.EXISTS-POS(pos) ?? pos !! ()) !! pos
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$k!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'k', $k)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$v!) is raw {
    $v
      ?? (SELF.EXISTS-POS(pos) ?? nqp::decont(SELF.AT-POS(pos)) !! ())
      !! SELF.AT-POS(pos)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$v!, *%_) is raw {
    Array::Element.access(SELF, pos, %_, 'v', $v)
}

# @a[$x]
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos) is raw {
    SELF.AT-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu \assignee) is raw {
    SELF.ASSIGN-POS(pos.Int, assignee)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu :$BIND! is raw) is raw {
    SELF.BIND-POS(pos.Int, $BIND)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$delete!) is raw {
    $delete ?? SELF.DELETE-POS(pos.Int) !! SELF.AT-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$delete!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'delete', $delete)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$exists!) is raw {
    $exists ?? SELF.EXISTS-POS(pos.Int) !! !SELF.EXISTS-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$exists!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'exists', $exists)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$kv!) is raw {
    $kv
      ?? (SELF.EXISTS-POS(pos.Int) ?? (pos, SELF.AT-POS(pos.Int)) !! ())
      !! (pos, SELF.AT-POS(pos.Int))
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$kv!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'kv', $kv)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$p!) is raw {
    $p
      ?? (SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos, SELF.AT-POS(pos.Int)) !! ())
      !! Pair.new(pos, SELF.AT-POS(pos.Int))
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$p!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'p', $p)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$k!) is raw {
    $k ?? (SELF.EXISTS-POS(pos.Int) ?? pos !! ()) !! pos
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$k!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'k', $k)
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, :$v!) is raw {
    $v
      ?? (SELF.EXISTS-POS(pos.Int) ?? nqp::decont(SELF.AT-POS(pos.Int)) !! ())
      !! SELF.AT-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D \pos, :$v!, *%_) is raw {
    Array::Element.access-any(SELF, pos, %_, 'v', $v)
}

# @a[@i]
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \positions, *%_) is raw {
    # MMD is not behaving itself so we do this by hand.
    return postcircumfix:<[ ]>(SELF, positions.Int, |%_)
      if nqp::iscont(positions);

    # Get the dispatch index
    my int $index;
    if nqp::getattr(%_,Map,'$!storage') {
        my $lookup := Rakudo::Internals.ADVERBS_TO_DISPATCH_INDEX(%_);
        if nqp::istype($lookup,X::Adverb) {
            $lookup.what   = "slice";
            $lookup.source = try { SELF.VAR.name } // SELF.^name;
            return Failure.new($lookup);
        }

        # Good to go!
        $index = $lookup;
    }

    # Do the correct processing for given dispatch index
    (positions.is-lazy
      ?? Rakudo::Internals.LAZY-ACCESS-SLICE-DISPATCH-CLASS($index)
      !! Rakudo::Internals.ACCESS-SLICE-DISPATCH-CLASS($index)
    ).new(SELF).slice(positions.iterator)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \positions, \values) is raw {
    # MMD is not behaving itself so we do this by hand.
    return postcircumfix:<[ ]>(SELF, positions.Int, values)
      if nqp::iscont(positions);

    my \iterator := positions.iterator;
    (iterator.is-lazy
      ?? Array::Slice::Assign::lazy-none
      !! Array::Slice::Assign::none
    ).new(
      SELF, Rakudo::Iterator.TailWith(values.iterator, Nil)
    ).assign-slice(iterator)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \positions, :$BIND! is raw) is raw {
    # MMD is not behaving itself so we do this by hand.
    return postcircumfix:<[ ]>(SELF, positions.Int, :$BIND)
      if nqp::iscont(positions);

    my \iterator := positions.iterator;
    (iterator.is-lazy
      ?? Array::Slice::Bind::lazy-none
      !! Array::Slice::Bind::none
    ).new(
      SELF, Rakudo::Iterator.TailWith($BIND.iterator, Nil)
    ).bind-slice(iterator)
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? SELF.AT-POS(pos)
      !! SELF[pos]
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? SELF.ASSIGN-POS(pos,assignee)
      !! (SELF[pos] = assignee)
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$delete!) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $delete
        ?? SELF.DELETE-POS(pos)
        !! SELF.AT-POS(pos)
      !! $delete
        ?? (SELF[pos]:delete)
        !! SELF[pos]
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$delete!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'delete', $delete)
      !! postcircumfix:<[ ]>(SELF, pos, :$delete, |%_)
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$exists!) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $exists
        ?? SELF.EXISTS-POS(pos)
        !! !SELF.EXISTS-POS(pos)
      !! (SELF[pos]:$exists)
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$exists!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'exists', $exists)
      !! postcircumfix:<[ ]>(SELF, pos, :$exists, |%_)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$kv!) is raw {
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $kv
        ?? SELF.EXISTS-POS(pos)
          ?? (pos,SELF.AT-POS(pos))
          !! ()
        !! (pos,SELF.AT-POS(pos))
      !! (SELF[pos]:$kv)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$kv!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'kv', $kv)
      !! postcircumfix:<[ ]>(SELF, pos, :$kv, |%_)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$p!) is raw {
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $p
        ?? SELF.EXISTS-POS(pos)
          ?? Pair.new(pos,SELF.AT-POS(pos))
          !! ()
        !! Pair.new(pos,SELF.AT-POS(pos))
      !! (SELF[pos]:$p)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$p!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'p', $p)
      !! postcircumfix:<[ ]>(SELF, pos, :$p, |%_)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$k!) is raw {
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $k
        ?? SELF.EXISTS-POS(pos)
          ?? pos
          !! ()
        !! pos
      !! (SELF[pos]:$k)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$k!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'k', $k)
      !! postcircumfix:<[ ]>(SELF, pos, :$k, |%_)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$v!) is raw {
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? $v
        ?? SELF.EXISTS-POS(pos)
          ?? SELF.AT-POS(pos)
          !! ()
        !! SELF.AT-POS(pos)
      !! (SELF[pos]:$v)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block, :$v!, *%_) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my \pos := $block.POSITIONS(SELF)),Int)
      ?? Array::Element.access-any(SELF, pos, %_, 'v', $v)
      !! postcircumfix:<[ ]>(SELF, pos, :$v, |%_)
}

# @a[*]
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, *%_) is raw {
    if nqp::getattr(%_,Map,'$!storage') {
        my $lookup := Rakudo::Internals.ADVERBS_TO_DISPATCH_INDEX(%_);
        if nqp::istype($lookup,X::Adverb) {
            $lookup.what   = "whatever slice";
            $lookup.source = try { SELF.VAR.name } // SELF.^name;
            return Failure.new($lookup);
        }
        return Rakudo::Internals.ACCESS-SLICE-DISPATCH-CLASS($lookup)
          .new(SELF).slice(Rakudo::Iterator.IntRange(0,SELF.end))
    }
    
    # fast path
    SELF.iterator.push-all(my $buffer := nqp::create(IterationBuffer));
    $buffer.List
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is raw {
    SELF[^SELF.elems] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}

# @a[**]
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, *%adv) is raw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, Mu \assignee) is raw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}

# @a[]
multi sub postcircumfix:<[ ]>(\SELF, *%_) is raw {

    # There are adverbs to check
    if nqp::getattr(%_,Map,'$!storage') {
        my $lookup := Rakudo::Internals.ADVERBS_TO_DISPATCH_INDEX(%_);
        if nqp::istype($lookup,X::Adverb) {
            $lookup.what   = "zen slice";
            $lookup.source = try { SELF.VAR.name } // SELF.^name;
            return Failure.new($lookup);
        }
        Rakudo::Internals.ACCESS-SLICE-DISPATCH-CLASS(
          $lookup
        ).new(SELF).slice(Rakudo::Iterator.IntRange(0,SELF.end))
    }

    # Just the thing, please
    else {
        SELF<>
    }
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is raw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}

# vim: expandtab shiftwidth=4
