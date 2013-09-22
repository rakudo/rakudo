# all sub postcircumfix [] candidates here please

sub POSITIONS (\SELF, \pos) {
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
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is rw {  # NOT SELECTED
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
multi sub postcircumfix:<[ ]>( \SELF, $pos ) is rw {    # NOT SELECTED
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
multi sub postcircumfix:<[ ]>( \SELF, Positional \pos ) is rw {  # NOT SELECTED
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
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete!,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists!,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv!,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p!,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k!,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Positional \pos,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v!
) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos),
     True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>( \SELF, Callable $block ) is rw { # NOT SELECTED
    SELF[$block(|(SELF.elems xx $block.count))];
}
multi sub postcircumfix:<[ ]>(\SELF, Callable $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete!,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists!,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists = $default,
  :$kv!,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p!,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k!,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Callable $block,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v!
) is rw {
    SLICE_MORE( SELF, $block(|(SELF.elems xx $block.count)),
      True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever ) is rw {  # NOT SELECTED
    SELF[SELF.keys];
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete!,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists!,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv!,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p!,
  :$k      = $default,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k!,
  :$v      = $default
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v!
) is rw {
   SLICE_MORE( SELF, SELF.keys,
     True, $delete, $exists, $kv, $p, $k, $v );
}

# @a[]
multi sub postcircumfix:<[ ]>( \SELF ) is rw {  # NOT SELECTED
    SELF.list;
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete!,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists!,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv!,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p!,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k!,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
multi sub postcircumfix:<[ ]>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v!
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      True, $delete, $exists, $kv, $p, $k, $v );
}
