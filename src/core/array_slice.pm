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
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[Int 1]
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos ) is rw {
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D $pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[$x]
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos ) is rw {
    SELF.AT-POS(pos.Int);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS(pos.Int, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS(pos.Int, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Positional:D \pos ) is rw {
    nqp::iscont(pos)
      ?? SELF.AT-POS(pos.Int)
      !! POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel;
}
multi sub postcircumfix:<[ ]>( \SELF, Positional:D \pos, Mu \assignee ) is rw {
    nqp::iscont(pos)
      ?? SELF.ASSIGN-POS(pos.Int,assignee)
      !! POSITIONS(SELF,pos).map({ SELF[$_] }).eager.Parcel = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$SINK!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional:D \pos,:$delete!,*%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Positional:D \pos,:$exists!,*%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$kv!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$p!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$k!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Positional:D \pos, :$v!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF,pos), :$v, |%other );
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
    SLICE_MORE_LIST( SELF, POSITIONS(SELF,$block), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$delete!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$delete, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$exists!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$exists, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$kv!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$kv, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$p!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$p, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$k!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$k, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$v!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$v, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D ) is rw {
    SELF[^SELF.elems];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is rw {
    SELF[^SELF.elems] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$SINK!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$delete!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$exists!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$kv!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$p!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$k!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$v!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$v, |%other );
}

# @a[**]
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, *%adv) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, Mu \assignee) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}

# @a[]
multi sub postcircumfix:<[ ]>( \SELF ) is rw {
    nqp::decont(SELF);
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, :$SINK!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$delete!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$exists!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$kv!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$k!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$v!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$v, |%other );
}

# @a[;]
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL:D \keys, *%adv) is rw {
    if keys > 1 {
        X::NYI.new(feature => "Accessing dimensions after HyperWhatever").throw
            if keys[0].isa(HyperWhatever);

        if [||] %adv<kv p k> {
            (SELF[keys[0]]:kv).map(-> \key, \value {
                # make sure to call .item so that recursive calls don't map the LoL's elems
                map %adv<kv> ?? -> \key2, \value2 { LoL.new(key, |key2).item, value2 } !!
                    %adv<p>  ?? {; LoL.new(key, |.key) => .value } !!
                    { LoL.new(key, |$_).item },
                    postcircumfix:<[ ]>(value, LoL.new(|keys[1..*]), |%adv);
            }).eager.Parcel;
        } else {
            (keys[0].isa(Whatever)
                ?? SELF[^SELF.elems].Parcel
                !! SELF[keys[0].list].Parcel
            ).map(-> \elem {
                postcircumfix:<[ ]>(elem, LoL.new(|keys[1..*]), |%adv);
            }).eager.Parcel;
        }
    } else {
        postcircumfix:<[ ]>(SELF, keys[0].elems > 1 ?? keys[0].list !! keys[0] , |%adv);
    }
}
multi sub postcircumfix:<[ ]> (\SELF is rw, LoL:D \keys, Mu \assignee) is rw {
    (SELF[keys],) = assignee;
}

# vim: ft=perl6 expandtab sw=4
