# all sub postcircumfix [] candidates here please

proto sub postcircumfix:<[ ]>(|) { * }

# @a[pos]
multi sub postcircumfix:<[ ]>(\SELF, $pos, Mu :$BIND! is parcel) is rw {
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, $pos ) is rw { SELF.at_pos($pos) }
multi sub postcircumfix:<[ ]>(
  \SELF, $pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_ONE( SELF, $pos, True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[pos1 pos2 pos3]
multi sub postcircumfix:<[ ]>(\SELF, Positional \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos ) is rw {
    nqp::iscont(pos) 
      ?? SELF.at_pos(pos) 
      !! pos.map({ SELF[$_] }).eager.Parcel;
}
multi sub postcircumfix:<[ ]>(
  \SELF, Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, pos, True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[*]
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever ) is rw { SELF[SELF.keys] }
multi sub postcircumfix:<[ ]>(
  \SELF, Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys, True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[]
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>( \SELF ) { SELF }
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys, True, $delete, $exists, $kv, $p, $k, $v );
}
