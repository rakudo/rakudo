# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(|) { * }

# %h<key>
multi sub postcircumfix:<{ }>( \SELF, $key ) is rw {
    SELF.at_key($key);
}
multi sub postcircumfix:<{ }>(\SELF, $key, Mu :$BIND! is parcel) is rw {
    SELF.bind_key($key, $BIND);
}
multi sub postcircumfix:<{ }>(
  \SELF,
  $key,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_ONE( SELF, $key,
      False, $delete, $exists, $kv, $p, $k, $v );
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Positional \key ) is rw {
    nqp::iscont(key) 
      ?? SELF.at_key(key) 
      !! key.map({ SELF{$_} }).eager.Parcel;
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(
  \SELF,
  Positional \key,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, key,
      False, $delete, $exists, $kv, $p, $k, $v );
}

# %h{*}
multi sub postcircumfix:<{ }>( \SELF, Whatever ) is rw {
    SELF{SELF.keys};
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(
  \SELF,
  Whatever,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      False, $delete, $exists, $kv, $p, $k, $v );
}

# %h{}
multi sub postcircumfix:<{ }>( \SELF ) is rw {
    SELF;
}
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(
  \SELF,
  :$delete = $default,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SLICE_MORE( SELF, SELF.keys,
      False, $delete, $exists, $kv, $p, $k, $v );
}
