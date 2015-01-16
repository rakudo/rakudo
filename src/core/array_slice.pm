# all sub postcircumfix [] candidates here please

sub POSITIONS(\SELF, \pos) { # handle possible infinite slices
    my $positions := pos.flat;

    if nqp::istype(pos,Range)                         # a Range
      || $positions.infinite                          # an infinite list
      || ($positions.gimme(*) && $positions.infinite) # an infinite list now
    {
        my $list = SELF.list;
        $positions.map( {
            last if $_ >= $list.gimme( $_ + 1 );
            $_;
        } ).eager.Parcel;
    }
    else {
        $positions.map( {
            nqp::istype($_,Callable) ?? $_(|(SELF.elems xx $_.count)) !! $_
        } ).eager.Parcel;
    }
}

my class X::NYI { ... }

proto sub postcircumfix:<[ ]>(|) { * }

multi sub postcircumfix:<[ ]>( \SELF, Any:U $type, |c ) is rw {
    die "Indexing requires an instance, tried to do: {SELF.VAR.name}[ {$type.gist} ]";
}

# @a[int 1]
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is rw {
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, Mu \assignee ) is rw {
    SELF.assign_pos($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$SINK, |%other );
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

# @a[Int 1]
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos ) is rw {
    SELF.at_pos($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Mu \assignee ) is rw {
    SELF.assign_pos($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D $pos, Mu :$BIND! is parcel) is rw {
    SELF.bind_pos($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, $pos, True, :$v, |%other );
}

# @a[$x]
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos ) is rw {
    SELF.at_pos(pos.Int);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Mu \assignee ) is rw {
    SELF.assign_pos(pos.Int, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu :$BIND! is parcel) is rw {
    SELF.bind_pos(pos.Int, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$SINK!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$delete!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$exists!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$kv!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$p!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$k!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$v!, *%other ) is rw {
    SLICE_ONE( SELF, pos.Int, True, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Positional:D \pos ) is rw {
    nqp::iscont(pos)
      ?? SELF.at_pos(pos.Int)
      !! POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel;
}
multi sub postcircumfix:<[ ]>( \SELF, Positional:D \pos, Mu \assignee ) is rw {
    nqp::iscont(pos)
      ?? SELF.assign_pos(pos.Int,assignee)
      !! POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$SINK!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional:D \pos,:$delete!,*%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional:D \pos,:$exists!,*%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$kv!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$p!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$k!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$v!, *%other) is rw {
   SLICE_MORE( SELF, POSITIONS(SELF,pos), True, :$v, |%other );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))];
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$SINK!, *%other) is rw {
    SLICE_MORE( SELF, POSITIONS(SELF,$block), True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$delete!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$delete, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$exists!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$exists, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$kv!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$kv, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$p!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$p, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$k!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$k, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$v!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE(  SELF, $pos,  True, :$v, |%other )
      !! SLICE_MORE( SELF, @$pos, True, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D ) is rw {
    SELF[SELF.keys];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is rw {
    SELF[SELF.keys] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$SINK!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$delete!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$exists!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$kv!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$p!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$k!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$v!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$v, |%other );
}

# @a[**]
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, *%adv) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, Mu \assignee, *%adv) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}

# @a[]
multi sub postcircumfix:<[ ]>( \SELF ) is rw {
    SELF.list;
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, :$SINK!, *%other) is rw {
    SLICE_MORE( SELF, SELF.keys, True, :$SINK, |%other );
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

# @a[;]
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL:D \keys, *%adv) is rw {
    if keys > 1 {
        X::NYI.new(feature => "Accessing dimensions after HyperWhatever").throw
            if keys[0].isa(HyperWhatever);

        postcircumfix:<[ ]>(SELF, keys[0], :kv).map(-> \key, \value {
            if [||] %adv<kv p k> {
                map %adv<kv> ?? -> \key2, \value2 { LoL.new(key, |key2).item, value2 } !!
                    %adv<p>  ?? {; LoL.new(key, |.key) => .value } !!
                    # .item so that recursive calls don't map the LoL's elems
                    %adv<k>  ?? { LoL.new(key, |$_).item } !!
                    *, postcircumfix:<[ ]>(value, LoL.new(|keys[1..*]), |%adv);
            } else {
                postcircumfix:<[ ]>(value, LoL.new(|keys[1..*]), |%adv);
            }
        }).eager.Parcel;
    } else {
        postcircumfix:<[ ]>(SELF, keys[0].elems > 1 ?? keys[0].list !! keys[0] , |%adv);
    }
}
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL:D \keys, Mu \assignee, *%adv) is rw {
    if keys > 1 {
        postcircumfix:<[ ]>(SELF, keys, |%adv) = assignee;
    } else {
        postcircumfix:<[ ]>(SELF, keys[0], assignee, |%adv);
    }
}

# vim: ft=perl6 expandtab sw=4
