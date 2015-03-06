# all sub postcircumfix {} candidates here please

proto sub postcircumfix:<{ }>(|) { * }

# %h<key>
multi sub postcircumfix:<{ }>( \SELF, \key ) is rw {
    SELF.at_key(key);
}
multi sub postcircumfix:<{ }>(\SELF, \key, Mu \ASSIGN) is rw {
    SELF.assign_key(key, ASSIGN);
}
multi sub postcircumfix:<{ }>(\SELF, \key, Mu :$BIND! is parcel) is rw {
    SELF.bind_key(key, $BIND);
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$SINK!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$SINK, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$delete!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$delete, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$exists!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$exists, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$kv!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$kv, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$p!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$p, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$k!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$k, |%other );
}
multi sub postcircumfix:<{ }>( \SELF, \key, :$v!, *%other ) is rw {
    SLICE_ONE_HASH( SELF, key, :$v, |%other );
}

# %h<a b c>
multi sub postcircumfix:<{ }>( \SELF, Positional \key ) is rw {
    nqp::iscont(key)
      ?? SELF.at_key(key)
      !! key.map({ SELF{$_} }).eager.Parcel;
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, Mu \ASSIGN) is rw {
    (nqp::iscont(key)
      ?? SELF.at_key(key)
      !! key.map({ SELF{$_} }).eager.Parcel) = ASSIGN
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF,Positional \key, :$SINK!,*%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$SINK, |%other );
}
multi sub postcircumfix:<{ }>(\SELF,Positional \key, :$delete!,*%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF,Positional \key, :$exists!,*%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$kv!, *%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$p!, *%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$k!, *%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$v!, *%other) is rw {
    SLICE_MORE_HASH( SELF, \key, :$v, |%other );
}

# %h{*}
multi sub postcircumfix:<{ }>( \SELF, Whatever ) is rw {
    SELF{SELF.keys};
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, Mu \ASSIGN) is rw {
    SELF{SELF.keys} = ASSIGN;
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$SINK!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$SINK, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$delete!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$exists!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$kv!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$k!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$p, |%other );
}

# %h{}
multi sub postcircumfix:<{ }>( \SELF ) is rw {
    nqp::decont(SELF);
}
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<{ }>(\SELF, :$SINK!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$SINK, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$delete!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$delete, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$exists!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$exists, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$kv!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$kv, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$p, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$k!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$k, |%other );
}
multi sub postcircumfix:<{ }>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE_HASH( SELF, SELF.keys, :$p, |%other );
}

# %h{;}
multi sub postcircumfix:<{ }> (\SELF is rw, LoL \keys, *%adv) is rw {
    if keys > 1 {
        X::NYI.new(feature => "Accessing dimensions after HyperWhatever").throw
            if keys[0].isa(HyperWhatever);

        if [||] %adv<kv p k> {
            postcircumfix:<{ }>(SELF, keys[0], :kv).map(-> \key, \value {
                map %adv<kv> ?? -> \key2, \value2 { LoL.new(key, |key2), value2 } !!
                    %adv<p>  ?? {; LoL.new(key, |.key) => .value } !!
                    # .item so that recursive calls don't map the LoL's elems
                    %adv<k>  ?? { LoL.new(key, |$_).item } !!
                    *, postcircumfix:<{ }>(value, LoL.new(|keys[1..*]), |%adv);
            }).eager.Parcel;
        } else {
            (keys[0].isa(Whatever)
                ?? SELF{SELF.keys}.Parcel
                !! SELF{keys[0].list}.Parcel
            ).map(-> \elem {
                postcircumfix:<{ }>(elem, LoL.new(|keys[1..*]), |%adv);
            }).eager.Parcel;
        }
    } else {
        postcircumfix:<{ }>(SELF, keys[0].elems > 1 ?? keys[0].list !! keys[0] , |%adv);
    }
}
multi sub postcircumfix:<{ }> (\SELF is rw, LoL \keys, Mu \ASSIGN) is rw {
    (SELF{keys},) = ASSIGN;
}

# vim: ft=perl6 expandtab sw=4
