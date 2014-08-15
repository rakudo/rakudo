# all sub postcircumfix [] candidates here please

sub POSITIONS (\SELF, \pos) { # handle possible infinite slices
    my $positions = pos.flat;
    $positions.gimme(*);
    return $positions.map( {
        $_ ~~ Callable ?? $_(|(SELF.elems xx $_.count)) !! $_
    } ).eager.Parcel unless $positions.infinite;

    my $list = SELF.list;
    $positions.map( {
        last if $_ >= $list.gimme( $_ + 1 );
        $_;
    } ).eager.Parcel;
}

proto sub postcircumfix:<[ ]>(|) { * }

# @a[1]
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is rw {
    fail X::Subscript::FromEnd.new(index => $pos, type => SELF.WHAT) if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, Mu \assignee ) is rw {
    X::Subscript::FromEnd.new(index => $pos, type => SELF.WHAT).throw if $pos < 0;
    SELF.assign_pos($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    fail X::Subscript::FromEnd.new(index => $pos, type => SELF.WHAT) if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$v, |%other );
}

# @a[$x]
multi sub postcircumfix:<[ ]>( \SELF, \pos ) is rw {
    fail X::Subscript::FromEnd.new(index => pos, type => SELF.WHAT) if pos < 0;
    SELF.at_pos(pos);
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, Mu \assignee ) is rw {
    X::Subscript::FromEnd.new(index => pos, type => SELF.WHAT).throw if pos < 0;
    SELF.assign_pos(pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, \pos, Mu :$BIND! is parcel) is rw {
    fail X::Subscript::FromEnd.new(index => pos, type => SELF.WHAT) if pos < 0;
    SELF.bind_pos(pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$SINK!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, \pos, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, pos, True, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos ) is rw {
    if nqp::iscont(pos)  {
        fail X::Subscript::FromEnd.new(index => pos, type => SELF.WHAT) if pos < 0;
        SELF.at_pos(pos);
    }
    else {
        POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel;
    }
}
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos, Mu \assignee ) is rw {
    if nqp::iscont(pos)  {
        fail X::Subscript::FromEnd.new(index => pos, type => SELF.WHAT) if pos < 0;
        SELF.assign_pos(pos, assignee);
    }
    else {
        POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel = assignee;
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$SINK!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional \pos,:$delete!,*%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional \pos,:$exists!,*%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$kv!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$p!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$k!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$v!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$v, |%other );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>( \SELF, Callable $block ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))];
}
multi sub postcircumfix:<[ ]>( \SELF, Callable $block, Mu \assignee ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$SINK!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,$block), True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable $block,:$delete!,*%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$delete, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable $block,:$exists!,*%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$exists, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$kv!, *%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$kv, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$p!, *%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$p, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$k!, *%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$k, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$v!, *%other) is rw {
   my @positions := POSITIONS(SELF,$block);
   +@positions == 1
     ?? SLICE_ONE(  SELF, @positions[0], True, :$v, |%other )
     !! SLICE_MORE( SELF, @positions,    True, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever ) is rw {
    SELF[SELF.keys];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever, Mu \assignee ) is rw {
    SELF[SELF.keys] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$SINK!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$delete!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$exists!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$kv!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$p!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$k!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$v!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$v, |%other );
}

# @a[]
multi sub postcircumfix:<[ ]>( \SELF ) is rw {
    SELF.list;
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, :$SINK!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$delete!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$exists!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$kv!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$p!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$k!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$v!, *%other) is rw {
   SLICE_MORE( SELF, SELF.keys, True, :$v, |%other );
}

# @a[;]
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL \keys, *%adv) is rw {
    if keys > 1 {
        postcircumfix:<[ ]>(SELF, keys[0], :kv).map(-> \key, \value {
            map %adv<kv> ?? -> \key2, \value2 { LoL.new(key, |key2), value2 } !!
                %adv<p>  ?? {; LoL.new(key, |.key) => .value } !!
                # .item so that recursive calls don't map the LoL's elems
                %adv<k>  ?? { LoL.new(key, |$_).item } !!
                *,
            postcircumfix:<[ ]>(value, LoL.new(|keys[1..*]), |%adv);
        }).eager;
    } else {
        postcircumfix:<[ ]>(SELF, keys[0], |%adv);
    }
}
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL \keys, Mu \assignee, *%adv) is rw {
    if keys > 1 {
        postcircumfix:<[ ]>(SELF, keys[0]).map({
            postcircumfix:<[ ]>($_, LoL.new(|keys[1..*]), |%adv)
        }).eager.Parcel = assignee;
    } else {
        postcircumfix:<[ ]>(SELF, keys[0], assignee, |%adv);
    }
}

# vim: ft=perl6 expandtab sw=4
