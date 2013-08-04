my class MapIter { ... }
my class Range { ... }
my class X::Bind::Slice { ... }
my class X::Bind::ZenSlice { ... }

my class Any {
    my $default= [];  # so that we can check passing of parameters to ".hash"
    multi method ACCEPTS(Any:D: Mu \a) { self === a }

    ########
    # List-like methods for Any.
    ########

    # primitives
    method infinite() { Nil }
    method list() {
        nqp::p6list(
          self.DEFINITE ?? nqp::list(self) !! nqp::list(), List, Mu
        );
    }
    method flat() {
        nqp::p6list(
          self.DEFINITE ?? nqp::list(self) !! nqp::list(), List, Bool::True
        );
    }
    method eager() {
        nqp::p6list(
          self.DEFINITE ?? nqp::list(self) !! nqp::list(), List, Bool::True
        ).eager;
    }
    method hash() {
        my % = self.DEFINITE ?? self !! ();
    }

    # derived from .list
    method elems() { self.list.elems }
    method end()   { self.list.end }
    method uniq(|c) { self.list.uniq(|c) }
    method squish(|c) { self.list.squish(|c) }
    method pick($n = 1) { self.list.pick($n) }
    method roll($n = 1) { self.list.roll($n) }
    method reverse() { self.list.reverse }
    method sort($by = &infix:<cmp>) { self.list.sort($by) }
    method values() { self.list }
    method keys()   { self.list.keys }
    method kv()     { self.list.kv }
    method pairs()  { self.list.pairs }
    method reduce(&with) { self.list.reduce(&with) }

    proto method classify(|) { * }
    multi method classify($test)   {
        {}.classify-list( $test, self.list );
    }
    multi method classify($test, :$into!)   {
        ( $into // $into.new ).classify-list( $test, self.list );
    }

    proto method categorize(|) { * }
    multi method categorize($test) {
        {}.categorize-list( $test, self.list );
    }
    multi method categorize($test, :$into!) {
        ( $into // $into.new ).categorize-list( $test, self.list );
    }

    # derived from MapIter/list
    method lol()  {
        MapIter.new(self.list, { .item }, Mu).list
    }
    method map($block) is rw {
        MapIter.new(self, $block, Bool::True).list
    }
    proto method tree(|) { * }
    multi method tree(Any:U:) { self }
    multi method tree(Any:D:) { self.lol }
    multi method tree(Any:D: Cool $count as Int) {
        $count > 1
          ?? MapIter.new(self.list, { .tree($count-1).item }, Mu).list
          !! $count == 1
             ?? self.lol
             !! self
    }
    multi method tree(Any:D: &c) {
        MapIter.new(self.list, { .&c.item }, Mu).list
    }

    method Array() { Array.new(self.flat) }

    # auto-vivifying
    proto method push(|) { * }
    multi method push(Any:U \SELF: *@values) {
        &infix:<=>(SELF, Array.new);
        SELF.push(@values);
    }

    proto method unshift(|) { * }
    multi method unshift(Any:U \SELF: *@values) {
        &infix:<=>(SELF, Array.new);
        SELF.unshift(@values);
    }

    method grep(Mu $test) is rw {
        self.map({ $_ if $_ ~~ $test });
    }
    method first(Mu $test) is rw {
        my @results := self.grep($test);
        @results ?? @results[0] !! Nil;
    }

    method join($separator = '') {
        my $list = (self,).flat.eager;
        my Mu $rsa := nqp::list_s();
        $list.gimme(4);        # force reification of at least 4 elements
        unless $list.infinite {  # presize array
            nqp::setelems($rsa, nqp::unbox_i($list.elems));
            nqp::setelems($rsa, 0);
        }
        my $tmp;
        while $list.gimme(0) {
            $tmp := $list.shift;
            nqp::push_s($rsa,
              nqp::unbox_s(nqp::istype($tmp, Str) ?? $tmp !! $tmp.Str));
        }
        nqp::push_s($rsa, '...') if $list.infinite;
        nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Str), $rsa))
    }

    method min($by = &infix:<cmp>) {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) }
        my $min;
        for self { 
            $min = $_ if .defined and !$min.defined || $cmp($_, $min) < 0;
        }
        $min // +$Inf;
    }

    method max($by = &infix:<cmp>) {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) }
        my $max;
        for self { 
            $max = $_ if .defined and !$max.defined || $cmp($_, $max) > 0;
        }
        $max // -$Inf;
    }

    method minmax($by = &infix:<cmp>) {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) };

        my $min;
        my $max;
        my $excludes_min = Bool::False;
        my $excludes_max = Bool::False;

        for @.list {
            .defined or next;

            if .isa(Range) {
                if !$min.defined || $cmp($_.min, $min) < 0 {
                    $min = $_;
                    $excludes_min = $_.excludes_min;
                }
                if !$max.defined || $cmp($_.max, $max) > 0 {
                    $max = $_;
                    $excludes_max = $_.excludes_max;
                }
            } else {
                if !$min.defined || $cmp($_, $min) < 0 {
                    $min = $_;
                    $excludes_min = Bool::False;
                }
                if !$max.defined || $cmp($_, $max) > 0 {
                    $max = $_;
                    $excludes_max = Bool::False;
                }
            }
        }
        Range.new($min // +$Inf,
                  $max // -$Inf,
                  :excludes_min($excludes_min),
                  :excludes_max($excludes_max));
    }

    # []
    sub RWPAIR(\k, \v) {
        my \p := nqp::create(Pair);
        nqp::bindattr(p, Enum, '$!key', k);
        nqp::bindattr(p, Enum, '$!value', v);
        p
    }
    
    proto method postcircumfix:<[ ]>(|) { * }
    multi method postcircumfix:<[ ]>() { self.list }
    multi method postcircumfix:<[ ]>(:$kv!) { self.kv }
    multi method postcircumfix:<[ ]>(:$p!) { self.pairs }
    multi method postcircumfix:<[ ]>(:$k!) { self.keys }
    multi method postcircumfix:<[ ]>(:$BIND!) {
        X::Bind::ZenSlice.new(type => self.WHAT).throw
    }

    # @a[$x]
    multi method postcircumfix:<[ ]>(\SELF: $pos) is rw {
        fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
        SELF.at_pos($pos);
    }
    multi method postcircumfix:<[ ]>($pos, Mu :$BIND! is parcel) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.bind_pos($pos, $BIND);
    }
    multi method postcircumfix:<[ ]>($pos, :$kv!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $kv & !self.exists($pos) ?? () !! ($pos, self.at_pos($pos));
    }
    multi method postcircumfix:<[ ]>($pos, :$p!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $p & !self.exists($pos) ?? () !! RWPAIR($pos, self.at_pos($pos));
    }
    multi method postcircumfix:<[ ]>($pos, :$k!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $k & !self.exists($pos) ?? () !! $pos;
    }
    multi method postcircumfix:<[ ]>($pos, :$v!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $v & !self.exists($pos) ?? () !! self.at_pos($pos);
    }

    # @a[1]
    multi method postcircumfix:<[ ]>(\SELF: int $pos) is rw {
        fail "Cannot use negative index $pos on {SELF.WHAT.perl}" if $pos < 0;
        SELF.at_pos($pos);
    }
    multi method postcircumfix:<[ ]>(int $pos, Mu :$BIND! is parcel) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.bind_pos($pos, $BIND);
    }
    multi method postcircumfix:<[ ]>(int $pos, :$kv!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $kv & !self.exists($pos) ?? () !! ($pos, self.at_pos($pos));
    }
    multi method postcircumfix:<[ ]>(int $pos, :$p!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $p & !self.exists($pos) ?? () !! RWPAIR($pos, self.at_pos($pos));
    }
    multi method postcircumfix:<[ ]>(int $pos, :$k!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $k & !self.exists($pos) ?? () !! $pos;
    }
    multi method postcircumfix:<[ ]>(int $pos, :$v!) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        $v & !self.exists($pos) ?? () !! self.at_pos($pos);
    }

    # @a[@i]
    multi method postcircumfix:<[ ]>(\SELF: Positional \pos) is rw {
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
    multi method postcircumfix:<[ ]>(Positional $pos, :$BIND!) is rw {
        X::Bind::Slice.new(type => self.WHAT).throw;
    }
    multi method postcircumfix:<[ ]>(\SELF: Positional \pos, :$kv!) is rw {
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
    multi method postcircumfix:<[ ]>(\SELF: Positional \pos, :$p!) is rw {
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
    multi method postcircumfix:<[ ]>(\SELF: Positional \pos, :$k!) is rw {
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
    multi method postcircumfix:<[ ]>(\SELF: Positional \pos, :$v!) is rw {
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
    multi method postcircumfix:<[ ]>(\SELF: Callable $block) is rw {
        SELF[$block(|(SELF.elems xx $block.count))]
    }
    multi method postcircumfix:<[ ]>(Callable $block, :$BIND!) is rw {
        X::Bind::Slice.new(type => self.WHAT).throw;
    }
    multi method postcircumfix:<[ ]>(\SELF: Callable $block, :$kv!) is rw {
        SELF[$block(|(SELF.elems xx $block.count))]:$kv
    }
    multi method postcircumfix:<[ ]>(\SELF: Callable $block, :$p!) is rw {
        SELF[$block(|(SELF.elems xx $block.count))]:$p
    }
    multi method postcircumfix:<[ ]>(\SELF: Callable $block, :$k!) is rw {
        SELF[$block(|(SELF.elems xx $block.count))]:$k
    }
    multi method postcircumfix:<[ ]>(\SELF: Callable $block, :$v!) is rw {
        SELF[$block(|(SELF.elems xx $block.count))]:$v
    }

    # @a[*]
    multi method postcircumfix:<[ ]>(\SELF: Whatever) is rw {
        SELF[^SELF.elems]
    }
    multi method postcircumfix:<[ ]>(Whatever, :$BIND!) is rw {
        X::Bind::Slice.new(type => self.WHAT).throw;
    }
    multi method postcircumfix:<[ ]>(\SELF: Whatever, :$kv!) is rw {
        SELF[^SELF.elems]:$kv
    }
    multi method postcircumfix:<[ ]>(\SELF: Whatever, :$p!) is rw {
        SELF[^SELF.elems]:$p
    }
    multi method postcircumfix:<[ ]>(\SELF: Whatever, :$k!) is rw {
        SELF[^SELF.elems]:$k
    }
    multi method postcircumfix:<[ ]>(\SELF: Whatever, :$v!) is rw {
        SELF[^SELF.elems]:$v
    }

    proto method at_pos(|) {*}
    multi method at_pos(Any:D: $pos) {
        fail X::OutOfRange.new(
            what => 'Index',
            got  => $pos,
            range => (0..0)
        ) if $pos != 0;
        self;
    }
    multi method at_pos(Any:U \SELF: $pos) is rw {
        nqp::bindattr(my $v, Scalar, '$!whence',
            -> { SELF.defined || &infix:<=>(SELF, Array.new);
                 SELF.bind_pos($pos, $v) });
        $v
    }
    
    method all() { all(self.list) }
    method any() { any(self.list) }
    method one() { one(self.list) }
    method none() { none(self.list) }

    ########
    # Hash-like methods for Any.
    ########
    proto method postcircumfix:<{ }>(|) { * }
    multi method postcircumfix:<{ }>() { self }
    multi method postcircumfix:<{ }>(:$kv!) { self.kv }
    multi method postcircumfix:<{ }>(:$p!) { self.pairs }
    multi method postcircumfix:<{ }>(:$k!) { self.keys }
    multi method postcircumfix:<{ }>(:$v!) { self.values }
    multi method postcircumfix:<{ }>(:$BIND!) {
        X::Bind::ZenSlice.new(type => self.WHAT).throw
    }

    # %h<key>
    multi method postcircumfix:<{ }>(\SELF: $key) is rw {
        SELF.at_key($key)
    }
    multi method postcircumfix:<{ }>(\SELF: $key, Mu :$BIND! is parcel) is rw {
        SELF.bind_key($key, $BIND)
    }
    multi method postcircumfix:<{ }>(
      \SELF: $key,
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
    multi method postcircumfix:<{ }>(
      \SELF: $key,
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
    multi method postcircumfix:<{ }>(\SELF: $key, :$kv!) is rw {
        !$kv | SELF.exists($key) ?? ( $key, SELF.at_key($key) ) !! ();
    }
    multi method postcircumfix:<{ }>(\SELF: $key, :$p!) is rw {
        !$p | SELF.exists($key) ?? RWPAIR( $key, SELF.at_key($key) ) !! ();
    }
    multi method postcircumfix:<{ }>(\SELF: $key, :$k!) is rw {
        !$k | SELF.exists($key) ?? $key !! ();
    }
    #there is no multi method postcircumfix:<{ }>(\SELF: $key, :$v!)

    # %h<a b c>
    multi method postcircumfix:<{ }>(\SELF: Positional \key) is rw {
        nqp::iscont(key) 
          ?? SELF.at_key(key) 
          !! key.map({ SELF{$_} }).eager.Parcel
    }
    multi method postcircumfix:<{ }>(Positional $key, :$BIND!) is rw {
        X::Bind::Slice.new(type => self.WHAT).throw
    }
    multi method postcircumfix:<{ }>(
      \SELF: Positional \key,
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
    multi method postcircumfix:<{ }>(
      \SELF: Positional \key,
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
    multi method postcircumfix:<{ }>(\SELF: Positional \key, :$kv!) is rw {
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
    multi method postcircumfix:<{ }>(\SELF: Positional \key, :$p!) is rw {
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
    multi method postcircumfix:<{ }>(\SELF: Positional \key, :$k!) is rw {
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
    multi method postcircumfix:<{ }>(\SELF: Positional \key, :$v!) is rw {
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
    multi method postcircumfix:<{ }>(\SELF: Whatever) is rw {
        SELF{SELF.keys}
    }
    multi method postcircumfix:<{ }>(Whatever, :$BIND!) is rw {
        X::Bind::Slice.new(type => self.WHAT).throw
    }
    multi method postcircumfix:<{ }>(
      \SELF: Whatever,
      :$delete! where so $delete,
      :$exists = $default,
      :$kv     = $default,
      :$p      = $default,
      :$k      = $default,
      :$v      = $default
    ) is rw {
        SELF{SELF.keys}:$delete:$exists:$kv:$p:$k:$v;
    }
    multi method postcircumfix:<{ }>(
      \SELF: Whatever,
      :$exists!,
      :$kv = $default,
      :$p  = $default,
      :$k  = $default
    ) is rw {
        SELF{SELF.keys}:$exists:$kv:$p:$k
    }
    multi method postcircumfix:<{ }>(\SELF: Whatever, :$kv!) is rw {
        SELF.kv;
    }
    multi method postcircumfix:<{ }>(\SELF: Whatever, :$p!) is rw {
        SELF.pairs
    }
    multi method postcircumfix:<{ }>(\SELF: Whatever, :$k!) is rw {
        SELF.keys
    }
    multi method postcircumfix:<{ }>(\SELF: Whatever, :$v!) is rw {
        SELF.values
    }

    # internals
    proto method at_key(|) { * }
    multi method at_key(Any:D: $key) {
        fail "postcircumfix:<\{ \}> not defined for type {self.WHAT.perl}";
    }
    multi method at_key(Any:U \SELF: $key) is rw {
        nqp::bindattr(my $v, Scalar, '$!whence',
            -> { SELF.defined || &infix:<=>(SELF, Hash.new);
                 SELF.bind_key($key, $v) });
        $v
    }

    method FLATTENABLE_LIST() { 
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() { nqp::hash() }
}
Metamodel::ClassHOW.exclude_parent(Any);

# builtin ops
proto infix:<===>($?, $?) is pure { * }
multi infix:<===>($a?)    { Bool::True }
multi infix:<===>($a, $b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a.WHICH), nqp::unbox_s($b.WHICH)))
}

proto infix:<before>($, $?)  is pure { * }
multi infix:<before>($x?)      { Bool::True }
multi infix:<before>(\a, \b)   { (a cmp b) < 0 }

proto infix:<after>($, $?) is pure { * }
multi infix:<after>($x?)       { Bool::True }
multi infix:<after>(\a, \b)    { (a cmp b) > 0 }

# XXX: should really be '$a is rw' (no \) in the next four operators
proto prefix:<++>(|)             { * }
multi prefix:<++>(Mu:D \a is rw) { a = a.succ }
multi prefix:<++>(Mu:U \a is rw) { a = 1 }
proto prefix:<-->(|)             { * }
multi prefix:<-->(Mu:D \a is rw) { a = a.pred }
multi prefix:<-->(Mu:U \a is rw) { a = -1 }

proto postfix:<++>(|)             { * }
multi postfix:<++>(Mu:D \a is rw) { my $b = a; a = a.succ; $b }
multi postfix:<++>(Mu:U \a is rw) { a = 1; 0 }
proto postfix:<-->(|)             { * }
multi postfix:<-->(Mu:D \a is rw) { my $b = a; a = a.pred; $b }
multi postfix:<-->(Mu:U \a is rw) { a = -1; 0 }

# builtins
proto infix:<min>(|) is pure { * }
multi infix:<min>(*@args) { @args.min }
# XXX the multi version suffers from a multi dispatch bug
# where the mandatory named is ignored in the presence of a slurpy
#proto sub min(|)     { * }
#multi sub min(*@args) { @args.min() }
#multi sub min(*@args, :&by!) { @args.min(&by) }
sub min(*@args, :&by = &infix:<cmp>) { @args.min(&by) }

proto infix:<max>(|) is pure { * }
multi infix:<max>(*@args) { @args.max }
#proto sub max(|) { * }
#multi sub max(*@args) { @args.max() }
#multi sub max(*@args, :&by!) { @args.max(&by) }
sub max(*@args, :&by = &infix:<cmp>) { @args.max(&by) }

proto infix:<minmax>(|) is pure { * }
multi infix:<minmax>(*@args) { @args.minmax }
#proto sub minmax(|) { * }
#multi sub minmax(*@args) { @args.minmax() }
#multi sub minmax(*@args, :&by!) { @args.minmax(&by) }
sub minmax(*@args, :&by = &infix:<cmp>) { @args.minmax(&by) }

proto map(|) {*}
multi map(&code, *@values) { @values.map(&code) }

proto grep(|) {*}
multi grep(Mu $test, *@values) { @values.grep($test) }

proto first(|) {*}
multi first(Mu $test, *@values) { @values.first($test) }

proto join(|) { * }
multi join($sep = '', *@values) { @values.join($sep) }

proto pick(|) { * }
multi pick($n, *@values) { @values.pick($n) }

proto roll(|) { * }
multi roll($n, *@values) { @values.roll($n) }

proto keys(|) { * }
multi keys($x) { $x.keys }

proto values(|) { * }
multi values($x) { $x.values }

proto pairs(|) { * }
multi pairs($x) { $x.pairs }

proto kv(|) { * }
multi kv($x) { $x.kv }

proto elems(|) { * }
multi elems($a) { $a.elems }

proto end(|) { * }
multi end($a) { $a.end }

proto classify(|) { * }
multi classify( $test, *@items ) { {}.classify-list( $test, @items ) }
#multi classify( $test, *@items, :$into! ) {   # problem in MMD
#    ( $into // $into.new).classify-list( $test, @items );
#}

proto categorize(|) { * }
multi categorize( $test, *@items ) { {}.categorize-list( $test, @items ) }
#multi categorize( $test, *@items, :$into! ) {   # problem in MMD
#    ( $into // $into.new).categorize-list( $test, @items );
#}

proto uniq(|) { * }
multi uniq(*@values, |c) { @values.uniq(|c) }

proto squish(|) { * }
multi squish(*@values, |c) { @values.squish(|c) }

proto sub sort(|) {*}
multi sub sort(*@values)      {
    @values.at_pos(0).^does(Callable)
        ?? do { my $cmp := @values.shift; @values.sort($cmp) }
        !!  @values.sort;
}

proto sub item(|) is pure { * }
multi sub item(*@a) { my $ = @a }
multi sub item(Mu $a) { $a }
