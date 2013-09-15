my $default= [];  # so that we can check missing parameters

# []
sub RWPAIR(\k, \v) {
    my \p := nqp::create(Pair);
    nqp::bindattr(p, Enum, '$!key', k);
    nqp::bindattr(p, Enum, '$!value', v);
    p
}

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

########
# Hash-like methods for Any.
########
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
