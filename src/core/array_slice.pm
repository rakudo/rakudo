# all sub postcircumfix [] candidates here please

proto sub postcircumfix:<[ ]>(|) { * }

# @a[1]
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND!) is parcel {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is parcel {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  int $pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SLICE_ONE( SELF, $pos,
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[$x]
multi sub postcircumfix:<[ ]>(\SELF, $pos, Mu :$BIND!) is parcel {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, $pos ) is parcel {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  $pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is parcel {
    fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
    SLICE_ONE( SELF, $pos,
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[@i]
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$BIND!) is parcel {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos ) is parcel {
    if nqp::iscont(pos)  {
        fail "Cannot use negative index {pos} on {SELF.WHAT.perl}" if pos < 0;
        SELF.at_pos(pos);
    }
    else {
        pos.map({ SELF[$_] }).eager.Parcel;
    }
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is parcel {
    SLICE_MORE( SELF, pos,
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$BIND!) is parcel {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF, Callable $block ) is parcel {
    SELF[$block(|(SELF.elems xx $block.count))];
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is parcel {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[*]
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is parcel {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever ) is parcel {
    SELF[SELF.keys];
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is parcel {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[]
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is parcel {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF ) is parcel {
    SELF.list;
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is parcel {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
