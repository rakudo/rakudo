# all sub postcircumfix [] candidates here please

# @a[]
proto sub postcircumfix:<[ ]>(|) { * }
multi sub postcircumfix:<[ ]>(\SELF) { SELF.list }
multi sub postcircumfix:<[ ]>(\SELF, :$kv!) { SELF.kv }
multi sub postcircumfix:<[ ]>(\SELF, :$p!) { SELF.pairs }
multi sub postcircumfix:<[ ]>(\SELF, :$k!) { SELF.keys }
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw
}

# @a[$x]
multi sub postcircumfix:<[ ]>(\SELF, $pos) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, Mu :$BIND! is parcel) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, :$kv!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $kv & !SELF.exists($pos) ?? () !! ($pos, SELF.at_pos($pos));
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, :$p!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $p & !SELF.exists($pos) ?? () !! RWPAIR($pos, SELF.at_pos($pos));
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, :$k!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $k & !SELF.exists($pos) ?? () !! $pos;
}
multi sub postcircumfix:<[ ]>(\SELF, $pos, :$v!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $v & !SELF.exists($pos) ?? () !! SELF.at_pos($pos);
}

# @a[1]
multi sub postcircumfix:<[ ]>(\SELF, int $pos) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, :$kv!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $kv & !SELF.exists($pos) ?? () !! ($pos, SELF.at_pos($pos));
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, :$p!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $p & !SELF.exists($pos) ?? () !! RWPAIR($pos, SELF.at_pos($pos));
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, :$k!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $k & !SELF.exists($pos) ?? () !! $pos;
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, :$v!) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    $v & !SELF.exists($pos) ?? () !! SELF.at_pos($pos);
}

# @a[@i]
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos) is rw {
    if nqp::iscont(pos) {
        SELF.at_pos(pos);
    }
    else {
        my $positions = pos.flat;
        $positions.gimme(*);
        if $positions.infinite {
            my $list = SELF.list;
            $positions.map( {
                last if $_ >= $list.gimme($_ + 1);
                SELF[$_];
            } ).eager.Parcel;
        }
        else {
            $positions.map( { SELF[$_] } ).eager.Parcel;
        }
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional $pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$kv!) is rw {
    if nqp::iscont(pos) {
        SELF[pos]:$kv;
    }
    else {
        my $positions = pos.flat;
        $positions.gimme(*);
        if $positions.infinite {
            my $list = SELF.list;
            $positions.map( {
                last if $_ >= $list.gimme($_ + 1);
                SELF[$_]:$kv;
            } ).eager.Parcel;
        }
        else {
            $positions.map( { SELF[$_]:$kv } ).eager.Parcel;
        }
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$p!) is rw {
    if nqp::iscont(pos) {
        SELF[pos]:$p;
    }
    else {
        my $positions = pos.flat;
        $positions.gimme(*);
        if $positions.infinite {
            my $list = SELF.list;
            $positions.map( {
                last if $_ >= $list.gimme($_ + 1);
                SELF[$_]:$p;
            } ).eager.Parcel;
        }
        else {
            $positions.map( { SELF[$_]:$p } ).eager.Parcel;
        }
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$k!) is rw {
    if nqp::iscont(pos) {
        SELF[pos]:$k;
    }
    else {
        my $positions = pos.flat;
        $positions.gimme(*);
        if $positions.infinite {
            my $list = SELF.list;
            $positions.map( {
                last if $_ >= $list.gimme($_ + 1);
                SELF[$_]:$k;
            } ).eager.Parcel;
        }
        else {
            $positions.map( { SELF[$_]:$k } ).eager.Parcel;
        }
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$v!) is rw {
    if nqp::iscont(pos) {
        SELF[pos]:$v;
    }
    else {
        my $positions = pos.flat;
        $positions.gimme(*);
        if $positions.infinite {
            my $list = SELF.list;
            $positions.map( {
                last if $_ >= $list.gimme($_ + 1);
                SELF[$_]:$v;
            } ).eager.Parcel;
        }
        else {
            $positions.map( { SELF[$_]:$v } ).eager.Parcel;
        }
    }
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable $block) is rw {
    SELF[$block(|(SELF.elems xx $block.count))]
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$kv!) is rw {
    SELF[$block(|(SELF.elems xx $block.count))]:$kv
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$p!) is rw {
    SELF[$block(|(SELF.elems xx $block.count))]:$p
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$k!) is rw {
    SELF[$block(|(SELF.elems xx $block.count))]:$k
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$v!) is rw {
    SELF[$block(|(SELF.elems xx $block.count))]:$v
}

# @a[*]
multi sub postcircumfix:<[ ]>(\SELF, Whatever) is rw {
    SELF[^SELF.elems]
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$kv!) is rw {
    SELF[^SELF.elems]:$kv
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$p!) is rw {
    SELF[^SELF.elems]:$p
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$k!) is rw {
    SELF[^SELF.elems]:$k
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$v!) is rw {
    SELF[^SELF.elems]:$v
}
