# all sub postcircumfix [] candidates here please

sub POSITIONS (\SELF, \pos) { # handle possible infinite slices
    my $positions = pos.flat;
    $positions.gimme(*);
    return $positions.eager.Parcel unless $positions.infinite;

    my $list = SELF.list;
    $positions.map( {
        last if $_ >= $list.gimme( $_ + 1 );
        $_;
    } ).eager.Parcel;
}

proto sub postcircumfix:<[ ]>(|) { * }

# @a[1]
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
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
multi sub postcircumfix:<[ ]>( \SELF, $pos ) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, Mu :$BIND! is parcel) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, $pos, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos ) is rw {
    if nqp::iscont(pos)  {
        fail "Cannot use negative index {pos} on {SELF.WHAT.perl}" if pos < 0;
        SELF.at_pos(pos);
    }
    else {
        POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel;
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
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
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF,Callable $block,:$delete!,*%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable $block,:$exists!,*%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$kv!, *%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$p!, *%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$k!, *%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$v!, *%other) is rw {
   SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
     True, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever ) is rw {
    SELF[SELF.keys];
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
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
