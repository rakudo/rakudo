# all sub postcircumfix {} candidates here please

# %h{}
proto sub postcircumfix:<{ }>(|) { * }
multi sub postcircumfix:<{ }>(\SELF) { SELF }
multi sub postcircumfix:<{ }>(\SELF, :$kv!) { SELF.kv }
multi sub postcircumfix:<{ }>(\SELF, :$p!) { SELF.pairs }
multi sub postcircumfix:<{ }>(\SELF, :$k!) { SELF.keys }
multi sub postcircumfix:<{ }>(\SELF, :$v!) { SELF.values }
multi sub postcircumfix:<{ }>(\SELF, :$BIND!) {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw
}

# %h<key>
multi sub postcircumfix:<{ }>(\SELF, $key) is rw {
    SELF.at_key($key)
}
multi sub postcircumfix:<{ }>(\SELF, $key, Mu :$BIND! is parcel) is rw {
    SELF.bind_key($key, $BIND)
}
multi sub postcircumfix:<{ }>(
  \SELF, $key,
  :$delete! where so $delete,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    if $exists & $kv & $p & $k & $v === $default {  # :delete
        SELF.delete($key);
    }
    elsif $exists !=== $default {
        my $wasthere = SELF.exists($key);
        SELF.delete($key);

        if $kv & $p === $default {                  # :delete:exists?
            !( $wasthere ?^ $exists )
        }
        elsif $kv !=== $default {                   # :delete:exists?:kv?
            !$kv | $wasthere ?? ( $key, !( $wasthere ?^ $exists ) ) !! ();
        }
        elsif $p !=== $default {                    # :delete:exists?:p?
            !$p | $wasthere ?? RWPAIR($key, !($wasthere ?^ $exists) ) !! ();
        }
    }
    elsif $kv !=== $default {                       # :delete?:kv?
        !$kv | SELF.exists($key) ?? ( $key, SELF.delete($key) ) !! ();
    }
    elsif $p !=== $default {                        # :delete:p?
        !$p | SELF.exists($key) ?? RWPAIR($key, SELF.delete($key)) !! ();
    }
    elsif $k !=== $default {                        # :delete:k?
        !$k | SELF.exists($key) ?? ( SELF.delete($key); $key ) !! ();
    }
    else {                                          # :delete:v?
        !$v | SELF.exists($key) ?? SELF.delete($key) !! ();
    }
}
multi sub postcircumfix:<{ }>(
  \SELF, $key,
  :$exists!,
  :$kv = $default,
  :$p  = $default,
  :$k  = $default
) is rw {
    my $wasthere= SELF.exists($key);
    if $kv & $p & $k === $default {                 # :exists?
        !( $wasthere ?^ $exists )
    }
    elsif $kv !=== $default {                       # :exists?:kv?
        !$kv | $wasthere ?? ( $key, !( $wasthere ?^ $exists ) ) !! ();
    }
    else {                                          # :exists:p?
        !$p | $wasthere ?? RWPAIR($key, !( $wasthere ?^ $exists )) !! ();
    }
}
multi sub postcircumfix:<{ }>(\SELF, $key, :$kv!) is rw {
    !$kv | SELF.exists($key) ?? ( $key, SELF.at_key($key) ) !! ();
}
multi sub postcircumfix:<{ }>(\SELF, $key, :$p!) is rw {
    !$p | SELF.exists($key) ?? RWPAIR( $key, SELF.at_key($key) ) !! ();
}
multi sub postcircumfix:<{ }>(\SELF, $key, :$k!) is rw {
    !$k | SELF.exists($key) ?? $key !! ();
}
#there is no multi sub postcircumfix:<{ }>(\SELF, $key, :$v!)

# %h<a b c>
multi sub postcircumfix:<{ }>(\SELF, Positional \key) is rw {
    nqp::iscont(key) 
      ?? SELF.at_key(key) 
      !! key.map({ SELF{$_} }).eager.Parcel
}
multi sub postcircumfix:<{ }>(\SELF, Positional $key, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw
}
multi sub postcircumfix:<{ }>(
  \SELF, Positional \key,
  :$delete! where so $delete,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {

    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$delete:$exists:$kv:$p:$k:$v;
    }
    elsif $exists & $kv & $p & $k & $v === $default { # :delete
        key.map( { SELF.delete($_) } ).eager.Parcel;
    }

    elsif $exists !=== $default {
        # no need to initialize every iteration of map
        my $wasthere;

        if $p & $kv === $default {                    # :delete:exists?
            key.map( {
                SELF.delete($_) if $wasthere = SELF.exists($_);
                !( $wasthere ?^ $exists );
            } ).eager.Parcel
        }
        elsif $kv !=== $default {                     # :delete:exists?:kv?
            key.map( {
                SELF.delete($_) if $wasthere = SELF.exists($_);
                !$kv | $wasthere ?? ( $_, !( $wasthere ?^ $exists ) ) !! ()
            } ).eager.Parcel
        }
        elsif $p !=== $default {                      # :delete:exists?:p?
            key.map( {
                SELF.delete($_) if $wasthere = SELF.exists($_);
                !$p | $wasthere ?? RWPAIR($_,!($wasthere ?^ $exists)) !! ()
            } ).eager.Parcel
        }
    }
    elsif $kv !=== $default {                         # :delete:kv?
        $kv
          ?? key.map( {
                 SELF.exists($_) ?? ( $_, SELF.delete($_) ) !! ()
             } ).eager.Parcel
          !! key.map( {
                 ( $_, SELF.delete($_) )
             } ).eager.Parcel;
    }
    elsif $p !=== $default {                          # :delete:p?
        $p
          ?? key.map( {
                 SELF.exists($_) ?? RWPAIR($_, SELF.delete($_)) !! ()
             } ).eager.Parcel
          !! key.map( {
                 RWPAIR($_, SELF.delete($_))
             } ).eager.Parcel;
    }
    elsif $k !=== $default {                          # :delete:k?
        $k
          ?? key.map( {
                 SELF.exists($_) ?? ( SELF.delete($_); $_ ) !! ()
             } ).eager.Parcel
          !! key.map( {
                 SELF.delete($_); $_
             } ).eager.Parcel;
    }
    else {                                            # :delete:v?
        $v
          ?? key.map( {
                 SELF.exists($_) ?? SELF.delete($_) !! ()
             } ).eager.Parcel
          !! key.map( {
                 SELF.delete($_)
             } ).eager.Parcel;
    }
}
multi sub postcircumfix:<{ }>(
  \SELF, Positional \key,
  :$exists!,
  :$kv = $default,
  :$p  = $default,
) is rw {

    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$exists:$kv:$p;
    }
    if $kv & $p === $default {                      # :exists?
        key.map({ !( SELF.exists($_) ?^ $exists ) }).eager.Parcel;
    }
    elsif $kv !=== $default {                       # :exists?:kv?
        $kv
          ?? key.map( {
                 SELF.exists($_) ?? ( $_, $exists ) !! ()
             } ).eager.Parcel
          !! key.map( {
                 ( $_, !( SELF.exists($_) ?^ $exists ) )
             } ).eager.Parcel;
    }
    else {                                          # :exists:p?
        $p
          ?? key.map( {
                 SELF.exists($_) ?? RWPAIR( $_, $exists ) !! ()
             } ).eager.Parcel
          !! key.map( {
                 RWPAIR( $_, !( SELF.exists($_) ?^ $exists ) )
             } ).eager.Parcel;
    }
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$kv!) is rw {
    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$kv;
    }
    elsif $kv {
        key.map( {
            SELF.exists($_) ?? ($_, SELF.at_key($_)) !! ()
        } ).eager.Parcel
    }
    else {
        key.map( {
            ( $_, SELF.at_key($_) )
        } ).eager.Parcel;
    }
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$p!) is rw {
    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$p;
    }
    elsif $p {
        key.map( {
            SELF.exists($_) ?? RWPAIR($_, SELF.at_key($_)) !! ()
        } ).eager.Parcel
    }
    else {
        key.map( {
            RWPAIR( $_, SELF.at_key($_) )
        } ).eager.Parcel;
    }
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$k!) is rw {
    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$k;
    }
    elsif $k {
        key.map( { $_ if SELF.exists($_) } ).eager.Parcel
    }
    else {
        key
    }
}
multi sub postcircumfix:<{ }>(\SELF, Positional \key, :$v!) is rw {
    if nqp::iscont(key) { # handle single key immediately
        SELF{key}:$v;
    }
    elsif $v {
        key.map( { SELF.exists($_) ?? SELF.at_key($_) !! () } ).eager.Parcel
    }
    else {
        key.map( { SELF.at_key($_) } ).eager.Parcel;
    }
}

# %h{*}
multi sub postcircumfix:<{ }>(\SELF, Whatever) is rw {
    SELF{SELF.keys}
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw
}
multi sub postcircumfix:<{ }>(
  \SELF, Whatever,
  :$delete! where so $delete,
  :$exists = $default,
  :$kv     = $default,
  :$p      = $default,
  :$k      = $default,
  :$v      = $default
) is rw {
    SELF{SELF.keys}:$delete:$exists:$kv:$p:$k:$v;
}
multi sub postcircumfix:<{ }>(
  \SELF, Whatever,
  :$exists!,
  :$kv = $default,
  :$p  = $default,
  :$k  = $default
) is rw {
    SELF{SELF.keys}:$exists:$kv:$p:$k
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$kv!) is rw {
    SELF.kv;
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$p!) is rw {
    SELF.pairs
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$k!) is rw {
    SELF.keys
}
multi sub postcircumfix:<{ }>(\SELF, Whatever, :$v!) is rw {
    SELF.values
}
