# all sub postcircumfix [] candidates here please

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
    Rakudo::Internals.ACCESS-SLICE-DISPATCH-CLASS(
      $index
    ).new(SELF).slice(positions.iterator)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \positions, \values) is raw {
    # MMD is not behaving itself so we do this by hand.
    nqp::iscont(positions)
      ?? postcircumfix:<[ ]>(SELF, positions.Int, values)
      !! Array::Slice::Assign::none.new(
           SELF, Rakudo::Iterator.TailWith(values.iterator, Nil)
         ).assign-slice(positions.iterator)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \positions, :$BIND! is raw) is raw {
    # MMD is not behaving itself so we do this by hand.
    nqp::iscont(positions)
      ?? postcircumfix:<[ ]>(SELF, positions.Int, :$BIND)
      !! Array::Slice::Bind::none.new(
           SELF, Rakudo::Iterator.TailWith($BIND.iterator, Nil)
         ).bind-slice(positions.iterator)
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
