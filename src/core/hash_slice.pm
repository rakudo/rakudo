# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(|) { * }

# %h<key>
multi sub postcircumfix:<{ }>( \SELF, $key ) is rw {   # NOT SELECTED
    SELF.at_key($key);
}
multi sub postcircumfix:<{ }>(\SELF, $key, Mu :$BIND! is parcel) is rw {
    SELF.bind_key($key, $BIND);
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$delete, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$exists, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$kv, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$p, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$k, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, $key, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, $key, False, :$v, |%other );
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Positional \key ) is rw {  # NOT SELECTED
    nqp::iscont(key) 
      ?? SELF.at_key(key) 
      !! key.map({ SELF{$_} }).eager.Parcel;
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF,Positional \key,:$delete!,*%other) is rw {
    SLICE_MORE( SELF, \key, False, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF,Positional \key,:$exists!,*%other) is rw {
    SLICE_MORE( SELF, \key, False, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$kv!, *%other) is rw {
    SLICE_MORE( SELF, \key, False, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$p!, *%other) is rw {
    SLICE_MORE( SELF, \key, False, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$k!, *%other) is rw {
    SLICE_MORE( SELF, \key, False, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$v!, *%other) is rw {
    SLICE_MORE( SELF, \key, False, :$v, |%other );
}

# %h{*}
multi sub postcircumfix:<{ }>( \SELF, Whatever ) is rw {  # NOT SELECTED
    SELF{SELF.keys};
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$delete!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$exists!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$kv!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$k!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$p, |%other );
}

# %h{}
multi sub postcircumfix:<{ }>( \SELF ) is rw {  # NOT SELECTED
    SELF;
}
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, :$delete!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$exists!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$kv!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$k!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, False, :$p, |%other );
}
