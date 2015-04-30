my class MapIter                { ... }
my class Pair                   { ... }
my class Range                  { ... }
my class X::Bind                { ... }
my class X::Bind::Slice         { ... }
my class X::Bind::ZenSlice      { ... }
my class X::Item                { ... }
my class X::Match::Bool         { ... }
my class X::Pairup::OddNumber   { ... }
my class X::Subscript::Negative { ... }

my role  Numeric { ... }

my class Any { # declared in BOOTSTRAP
    # my class Any is Mu {

    multi method ACCEPTS(Any:D: Mu \a) { self === a }

    method invoke(|c) {
        DEPRECATED('CALL-ME',|<2015.03 2015.09>);
        self.CALL-ME(|c);
    }

    method exists_key(|c) is nodal {
        DEPRECATED('EXISTS-KEY',|<2015.03 2015.09>);
        self.EXISTS-KEY(|c);
    }

    proto method EXISTS-KEY(|) is nodal { * }
    multi method EXISTS-KEY(Any:U: $) is nodal { False }
    multi method EXISTS-KEY(Any:D: $) is nodal { False }

    method delete_key(|c) is nodal {
        DEPRECATED('DELETE-KEY',|<2015.03 2015.09>);
        self.DELETE-KEY(|c);
    }

    proto method DELETE-KEY(|) is nodal { * }
    multi method DELETE-KEY(Any:U: $) is nodal { Nil }
    multi method DELETE-KEY(Any:D: $) is nodal {
        fail "Can not remove values from a {self.^name}";
    }

    method delete_pos(|c) is nodal {
        DEPRECATED('DELETE-POS',|<2015.03 2015.09>);
        self.DELETE-POS(|c);
    }

    proto method DELETE-POS(|) is nodal { * }
    multi method DELETE-POS(Any:U: $pos) is nodal { Nil }
    multi method DELETE-POS(Any:D: $pos) is nodal {
        fail "Can not remove elements from a {self.^name}";
    }

    proto method list(|) is nodal { * }
    multi method list(Any:U:) is nodal { nqp::p6list(nqp::list(),     List, Mu) }
    multi method list(Any:D:) is nodal { nqp::p6list(nqp::list(self), List, Mu) }

    proto method flat(|) is nodal { * }
    multi method flat(Any:U:) is nodal { nqp::p6list(nqp::list(),     List, Bool::True) }
    multi method flat(Any:D:) is nodal { nqp::p6list(nqp::list(self), List, Bool::True) }

    proto method eager(|) is nodal { * }
    multi method eager(Any:U:) is nodal {
        nqp::p6list(nqp::list(),     List, Mu).eager;
    }
    multi method eager(Any:D:) is nodal {
        nqp::p6list(nqp::list(self), List, Mu).eager;
    }

    proto method hash(|) is nodal { * }
    multi method hash(Any:U:) is nodal { my % = () }
    multi method hash(Any:D:) is nodal { my % = self }
    method Hash() is nodal { self.hash }

    # derived from .list
    method Parcel() is nodal { self.list.Parcel }
    method List() is nodal { self.list }

    proto method elems(|) is nodal { * }
    multi method elems(Any:U:) is nodal { 0 }
    multi method elems(Any:D:) is nodal { self.list.elems }

    proto method end(|) is nodal { * }
    multi method end(Any:U:) is nodal { -1 }
    multi method end(Any:D:) is nodal { self.list.end }

    proto method keys(|) is nodal { * }
    multi method keys(Any:U:) is nodal { ().list }
    multi method keys(Any:D:) is nodal { self.list.keys }

    proto method kv(|) is nodal { * }
    multi method kv(Any:U:) is nodal { ().list }
    multi method kv(Any:D:) is nodal { self.list.kv }

    proto method values(|) is nodal { * }
    multi method values(Any:U:) is nodal { ().list }
    multi method values(Any:D:) is nodal { self.list }

    proto method pairs(|) is nodal { * }
    multi method pairs(Any:U:) is nodal { ().list }
    multi method pairs(Any:D:) is nodal { self.list.pairs }

    proto method antipairs(|) is nodal { * }
    multi method antipairs(Any:U:) is nodal { ().list }
    multi method antipairs(Any:D:) is nodal { self.list.antipairs }

    proto method invert(|) is nodal { * }

    proto method pairup(|) is nodal { * }
    multi method pairup(Any:U:) is nodal { ().list }
    multi method pairup(Any:D:) is nodal {

        my $list := self.list;
        my int $i;
        my int $elems = self.elems;

        gather while $i < $elems {
            my Mu $it := $list.AT-POS($i++);
            if nqp::istype($it,Enum) {
                take $it.key => $it.value;
            }
            elsif nqp::istype($it,EnumMap) and !nqp::iscont($it) {
                take $it.pairs;
            }
            elsif $i < $elems {
                take $it => $list.AT-POS($i++);
            }
            else {
                X::Pairup::OddNumber.new.throw;
            }
        }
    }

    method squish(|c) is nodal { self.list.squish(|c) }
    method rotor(|c) is nodal { self.list.rotor(|c) }
    method reverse() is nodal { self.list.reverse }
    method sort($by = &infix:<cmp>) is nodal { self.list.sort($by) }
    method reduce(&with) is nodal { self.list.reduce(&with) }
    method combinations(|c) is nodal { self.list.combinations(|c) }
    method permutations(|c) is nodal { self.list.permutations(|c) }

    method unique(|c) is nodal { self.list.unique(|c) }
    method uniq(|c) is nodal {
        DEPRECATED('unique', |<2014.11 2015.09>);
        self.unique(|c);
    }

    proto method pick(|) is nodal { * }
    multi method pick()   is nodal { self.list.pick     }
    multi method pick($n) is nodal { self.list.pick($n) }

    proto method roll(|) is nodal { * }
    multi method roll()   is nodal { self.list.roll     }
    multi method roll($n) is nodal { self.list.roll($n) }

    proto method classify(|) is nodal { * }
    multi method classify($test)   is nodal {
        Hash.^parameterize(Any,Any).new.classify-list( $test, self.list );
    }
    multi method classify($test, :$into!)   is nodal {
        ( $into // $into.new ).classify-list( $test, self.list );
    }

    proto method categorize(|) is nodal { * }
    multi method categorize($test) is nodal {
        Hash.^parameterize(Any,Any).new.categorize-list( $test, self.list );
    }
    multi method categorize($test, :$into!) is nodal {
        ( $into // $into.new ).categorize-list( $test, self.list );
    }

    # derived from MapIter/list
    method lol()  is nodal {
        MapIter.new(self.list, { .item }, Mu).list
    }
    proto method map (|) is nodal { * }
    multi method map(Whatever) is rw is nodal { self }
    multi method map($block, :$label) is rw is nodal {
        MapIter.new(self, $block, Bool::False, :$label).list
    }
    proto method FOR (|) { * }
    multi method FOR(Whatever) is rw { self }
    multi method FOR($block, :$label) is rw {
        MapIter.new(self, $block, Bool::False, :$label).list;
    }
    method for(|c) is nodal {
        DEPRECATED('flatmap',|<2015.05 2015.09>);
        self.flatmap(|c);
    }
    proto method flatmap (|) is nodal { * }
    multi method flatmap(Whatever) is rw is nodal { self }
    multi method flatmap($block, :$label) is rw is nodal {
        MapIter.new(self, $block, Bool::True, :$label).list
    }
    method nodemap($block) is rw is nodal { nodemap($block, self) }
    method duckmap($block) is rw is nodal { duckmap($block, self) }
    method deepmap($block) is rw is nodal { deepmap($block, self) }

    proto method tree(|) is nodal { * }
    multi method tree(Any:U:) is nodal { self }
    multi method tree(Any:D:) is nodal {
        nqp::istype(self,Positional)
            ?? LoL.new(|MapIter.new(self.list, { .tree }, Mu).list).item
            !! self
    }
    multi method tree(Any:D: Whatever ) is nodal { self.tree }
    multi method tree(Any:D: Int(Cool) $count) {
        nqp::istype(self,Positional) && $count > 0
            ?? LoL.new(|MapIter.new(self.list, { .tree($count - 1) }, Mu).list).item
            !! self
    }
    multi method tree(Any:D: *@ [&first, *@rest]) is nodal {
        nqp::istype(self,Positional)
            ?? @rest ?? first MapIter.new(self.list, { .tree(|@rest) }, Mu).list
                     !! first self.list
            !! self
    }

    method Array() is nodal { Array.new(self.flat) }

    # auto-vivifying
    proto method push(|) is nodal { * }
    multi method push(Any:U \SELF: *@values) is nodal {
        SELF = nqp::istype(SELF,Positional) ?? SELF.new !! Array.new;
        SELF.push(@values);
    }

    proto method unshift(|) is nodal { * }
    multi method unshift(Any:U \SELF: *@values) is nodal {
        SELF = Array.new;
        SELF.unshift(@values);
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) is rw is nodal {
        fail X::Match::Bool.new( type => '.grep' );
    }
    multi method grep(Regex:D $test) is rw is nodal {
        self.map({ next unless .match($test); $_ });
    }
    multi method grep(Callable:D $test) is rw is nodal {
        self.map({ next unless $test($_); $_ });
    }
    multi method grep(Mu $test) is rw is nodal {
        self.map({ next unless $_ ~~ $test; $_ });
    }

    proto method grep-index(|) is nodal { * }
    multi method grep-index(Bool:D $t) is rw is nodal {
        fail X::Match::Bool.new( type => '.grep-index' );
    }
    multi method grep-index(Regex:D $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index+1;
            next unless .match($test);
            nqp::box_i($index,Int);
        };
    }
    multi method grep-index(Callable:D $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            next unless $test($_);
            nqp::box_i($index,Int);
        };
    }
    multi method grep-index(Mu $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            next unless $_ ~~ $test;
            nqp::box_i($index,Int);
        };
    }

    proto method first(|) is nodal { * }
    multi method first(Bool:D $t) is rw is nodal {
        fail X::Match::Bool.new( type => '.first' );
    }
    multi method first(Regex:D $test) is rw is nodal {
        self.map({ return-rw $_ if .match($test) });
        Nil;
    }
    multi method first(Callable:D $test) is rw is nodal {
        self.map({ return-rw $_ if $test($_) });
        Nil;
    }
    multi method first(Mu $test) is rw is nodal {
        self.map({ return-rw $_ if $_ ~~ $test });
        Nil;
    }

    proto method first-index(|) is nodal { * }
    multi method first-index(Bool:D $t) is rw is nodal {
        fail X::Match::Bool.new( type => '.first-index' );
    }
    multi method first-index(Regex:D $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if .match($test);
        };
        Nil;
    }
    multi method first-index(Callable:D $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if $test($_);
        };
        Nil;
    }
    multi method first-index(Mu $test) is nodal {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if $_ ~~ $test;
        };
        Nil;
    }

    proto method last-index(|) is nodal { * }
    multi method last-index(Bool:D $t) is rw is nodal {
        fail X::Match::Bool.new( type => '.last-index' );
    }
    multi method last-index(Regex:D $test) is nodal {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if self.AT-POS($index).match($test);
        }
        Nil;
    }
    multi method last-index(Callable:D $test) is nodal {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if $test(self.AT-POS($index));
        }
        Nil;
    }
    multi method last-index(Mu $test) is nodal {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if self.AT-POS($index) ~~ $test;
        }
        Nil;
    }

    method join($separator = '') is nodal {
        my $list = (self,).eager;
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
              nqp::unbox_s(nqp::istype($tmp, Str) && nqp::isconcrete($tmp) ?? $tmp !! $tmp.Str));
        }
        nqp::push_s($rsa, '...') if $list.infinite;
        nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Str), $rsa))
    }

    proto method min (|) is nodal { * }
    multi method min(Any:D:) is nodal {
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $_ cmp $min < 0;
        }
        $min // Inf;
    }
    multi method min(Any:D: $by) is nodal {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) }
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $cmp($_, $min) < 0;
        }
        $min // Inf;
    }

    proto method max (|) is nodal { * }
    multi method max(Any:D:) is nodal {
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $_ cmp $max > 0;
        }
        $max // -Inf;
    }
    multi method max(Any:D: $by) is nodal {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) }
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $cmp($_, $max) > 0;
        }
        $max // -Inf;
    }

    proto method minmax (|) is nodal { * }
    multi method minmax(Any:D: $by = &infix:<cmp>) is nodal {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) };

        my $min;
        my $max;
        my $excludes-min = Bool::False;
        my $excludes-max = Bool::False;

        @.list.map: {
            .defined or next;

            if .isa(Range) {
                if !$min.defined || $cmp($_.min, $min) < 0 {
                    $min = .min;
                    $excludes-min = $_.excludes-min;
                }
                if !$max.defined || $cmp($_.max, $max) > 0 {
                    $max = .max;
                    $excludes-max = $_.excludes-max;
                }
            } elsif Positional.ACCEPTS($_) {
                my $mm = .minmax($by);
                if !$min.defined || $cmp($mm.min, $min) < 0 {
                    $min = $mm.min;
                    $excludes-min = $mm.excludes-min;
                }
                if !$max.defined || $cmp($mm.max, $max) > 0 {
                    $max = $mm.max;
                    $excludes-max = $mm.excludes-max;
                }
            } else {
                if !$min.defined || $cmp($_, $min) < 0 {
                    $min = $_;
                    $excludes-min = Bool::False;
                }
                if !$max.defined || $cmp($_, $max) > 0 {
                    $max = $_;
                    $excludes-max = Bool::False;
                }
            }
        }
        Range.new($min // Inf,
                  $max // -Inf,
                  :excludes-min($excludes-min),
                  :excludes-max($excludes-max));
    }

    method exists_pos(|c) is nodal {
        DEPRECATED('EXISTS-POS',|<2015.03 2015.09>);
        self.EXISTS-POS(|c);
    }

    proto method EXISTS-POS(|) is nodal { * }
    multi method EXISTS-POS(Any:U: Any:D $) is nodal { False }
    multi method EXISTS-POS(Any:U: Any:U $pos) is rw is nodal {
        die "Cannot use '{$pos.^name}' as an index";
    }

    multi method EXISTS-POS(Any:D: int \pos) is nodal {
        nqp::p6bool(nqp::iseq_i(pos,0));
    }
    multi method EXISTS-POS(Any:D: Int:D \pos) is nodal {
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Num:D \pos) is nodal {
        X::Item.new(aggregate => self, index => pos).throw
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int));
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Any:D \pos) is nodal {
        pos.Int == 0;
    }
    multi method EXISTS-POS(Any:D: Any:U \pos) is nodal {
        die "Cannot use '{pos.^name}' as an index";
    }

    method at_pos(|c) is rw is nodal {
        DEPRECATED('AT-POS',|<2015.03 2015.09>);
        self.AT-POS(|c);
    }

    proto method AT-POS(|) is nodal {*}
    multi method AT-POS(Any:U \SELF: int \pos) is rw is nodal {
        nqp::bindattr(my $v, Scalar, '$!whence',
            -> { SELF.defined || (SELF = Array.new);
                 SELF.BIND-POS(pos, $v) });
        $v
    }
    multi method AT-POS(Any:U \SELF: Int:D \pos) is rw is nodal {
        nqp::bindattr(my $v, Scalar, '$!whence',
            -> { SELF.defined || (SELF = Array.new);
                 SELF.BIND-POS(nqp::unbox_i(pos), $v) });
        $v
    }
    multi method AT-POS(Any:U: Num:D \pos) is rw is nodal {
        fail X::Item.new(aggregate => self, index => pos)
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int));
    }
    multi method AT-POS(Any:U: Any:D \pos) is rw is nodal {
        self.AT-POS(nqp::unbox_i(pos.Int));
    }

    multi method AT-POS(Any:D: int \pos) is rw is nodal {
        fail X::OutOfRange.new(:what<Index>, :got(pos), :range<0..0>)
          unless nqp::not_i(pos);
        self;
    }
    multi method AT-POS(Any:D: Int:D \pos) is rw is nodal {
        fail X::OutOfRange.new(:what<Index>, :got(pos), :range<0..0>)
          if pos != 0;
        self;
    }
    multi method AT-POS(Any:D: Num:D \pos) is rw is nodal {
        fail X::Item.new(aggregate => self, index => pos)
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int));
    }
    multi method AT-POS(Any:D: Any:D \pos) is rw is nodal {
        self.AT-POS(nqp::unbox_i(pos.Int));
    }

    multi method AT-POS(Any:   Any:U \pos) is rw is nodal {
        die "Cannot use '{pos.^name}' as an index";
    }

    method bind_pos(|c) is rw is nodal {
        DEPRECATED('BIND-POS',|<2015.03 2015.09>);
        self.BIND-POS(|c);
    }

    method assign_pos(|c) is nodal {
        DEPRECATED('ASSIGN-POS',|<2015.03 2015.09>);
        self.ASSIGN-POS(|c);
    }

    proto method ASSIGN-POS(|) is nodal { * }
    multi method ASSIGN-POS(Any:U \SELF: \pos, Mu \assignee) is nodal {
       SELF.AT-POS(pos) = assignee;                     # defer < 0 check
    }

    multi method ASSIGN-POS(Any:D: int \pos, Mu \assignee) is nodal {
        self.AT-POS(pos) = assignee;                    # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Int:D \pos, Mu \assignee) is nodal {
        self.AT-POS(pos) = assignee;                    # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Num:D \pos, Mu \assignee) is nodal {
        fail X::Item.new(aggregate => self, index => pos)
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Any:D \pos, Mu \assignee) is nodal {
        self.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Any:U \pos, Mu \assignee) is nodal {
        die "Cannot use '{pos.^name}' as an index";
    }

    method all() is nodal { Junction.new(self.list, :type<all>) }
    method any() is nodal { Junction.new(self.list, :type<any>) }
    method one() is nodal { Junction.new(self.list, :type<one>) }
    method none() is nodal { Junction.new(self.list, :type<none>) }

    method at_key(|c) is rw is nodal {
        DEPRECATED('AT-KEY',|<2015.03 2015.09>);
        self.AT-KEY(|c);
    }

    # internals
    proto method AT-KEY(|) is nodal { * }
    multi method AT-KEY(Any:D: $key) is rw is nodal {
        fail "postcircumfix:<\{ \}> not defined for type {self.WHAT.perl}";
    }
    multi method AT-KEY(Any:U \SELF: $key) is rw is nodal {
        nqp::bindattr(my $v, Scalar, '$!whence',
            -> { SELF.defined || (SELF = Hash.new);
                 SELF.BIND-KEY($key, $v) });
        $v
    }

    method bind_key(|c) is rw is nodal {
        DEPRECATED('BIND-KEY',|<2015.03 2015.09>);
        self.BIND-KEY(|c);
    }

    proto method BIND-KEY(|) is nodal { * }
    multi method BIND-KEY(Any:D: \k, \v) is rw is nodal {
        fail X::Bind.new(target => self.^name);
    }
    multi method BIND-KEY(Any:U \SELF: $key, $BIND ) is rw is nodal {
        SELF = Hash.new;
        SELF.BIND-KEY($key, $BIND);
        $BIND
    }

    method assign_key(|c) is nodal {
        DEPRECATED('ASSIGN-KEY',|<2015.03 2015.09>);
        self.ASSIGN-KEY(|c);
    }

    proto method ASSIGN-KEY(|) is nodal { * }
    multi method ASSIGN-KEY(\SELF: \key, Mu \assignee) is nodal {
        SELF.AT-KEY(key) = assignee;
    }

    method FLATTENABLE_LIST() is nodal {
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() is nodal { nqp::hash() }

    method Set()     is nodal {     Set.new-from-pairs(self.list) }
    method SetHash() is nodal { SetHash.new-from-pairs(self.list) }
    method Bag()     is nodal {     Bag.new-from-pairs(self.list) }
    method BagHash() is nodal { BagHash.new-from-pairs(self.list) }
    method Mix()     is nodal {     Mix.new-from-pairs(self.list) }
    method MixHash() is nodal { MixHash.new-from-pairs(self.list) }

    method Supply() is nodal { self.list.Supply }

    method print-nl() { self.print("\n") }
}
Metamodel::ClassHOW.exclude_parent(Any);

# builtin ops
proto sub infix:<===>(Mu $?, Mu $?) is pure { * }
multi sub infix:<===>($?)    { Bool::True }
multi sub infix:<===>($a, $b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a.WHICH), nqp::unbox_s($b.WHICH)))
}

proto sub infix:<before>(Mu $, Mu $?)  is pure { * }
multi sub infix:<before>($?)      { Bool::True }
multi sub infix:<before>(\a, \b)   { (a cmp b) < 0 }

proto sub infix:<after>(Mu $, Mu $?) is pure { * }
multi sub infix:<after>($x?)       { Bool::True }
multi sub infix:<after>(\a, \b)    { (a cmp b) > 0 }

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
proto sub infix:<min>(|) is pure { * }
multi sub infix:<min>(Mu:D \a, Mu:U) { a }
multi sub infix:<min>(Mu:U, Mu:D \b) { b }
multi sub infix:<min>(Mu:D \a, Mu:D \b) { (a cmp b) < 0 ?? a !! b }
multi sub infix:<min>(*@args) { @args.min }
# XXX the multi version suffers from a multi dispatch bug
# where the mandatory named is ignored in the presence of a slurpy
#proto sub min(|)     { * }
#multi sub min(*@args) { @args.min() }
#multi sub min(*@args, :&by!) { @args.min(&by) }
sub min(*@args, :&by = &infix:<cmp>) { @args.min(&by) }

proto sub infix:<max>(|) is pure { * }
multi sub infix:<max>(Mu:D \a, Mu:U) { a }
multi sub infix:<max>(Mu:U, Mu:D \b) { b }
multi sub infix:<max>(Mu:D \a, Mu:D \b) { (a cmp b) > 0 ?? a !! b }
multi sub infix:<max>(*@args) { @args.max }
#proto sub max(|) { * }
#multi sub max(*@args) { @args.max() }
#multi sub max(*@args, :&by!) { @args.max(&by) }
sub max(*@args, :&by = &infix:<cmp>) { @args.max(&by) }

proto sub infix:<minmax>(|) is pure { * }
multi sub infix:<minmax>(**@args) { @args.minmax }
#proto sub minmax(|) { * }
#multi sub minmax(*@args) { @args.minmax() }
#multi sub minmax(*@args, :&by!) { @args.minmax(&by) }
sub minmax(**@args, :&by = &infix:<cmp>) { @args.minmax(&by) }

proto sub map(|) {*}
# fails integration/99problems-21-to-30, test 12/13
#multi sub map(&code, @values) { @values.map(&code) }
multi sub map(&code, *@values) { @values.map(&code) }
multi sub map(Whatever, \a)    { a }
multi sub map(&code, Whatever) { (1..Inf).map(&code) }

proto sub grep(|) {*}
multi sub grep(Mu $test, @values) { @values.grep($test) }
multi sub grep(Mu $test, *@values) { @values.grep($test) }
multi sub grep(Bool:D $t, *@v) { fail X::Match::Bool.new( type => 'grep' ) }

proto sub grep-index(|) {*}
multi sub grep-index(Mu $test, @values) { @values.grep-index($test) }
multi sub grep-index(Mu $test, *@values) { @values.grep-index($test) }
multi sub grep-index(Bool:D $t, *@v) {
    fail X::Match::Bool.new(type => 'grep-index');
}

proto sub first(|) {*}
multi sub first(Mu $test, @values) { @values.first($test) }
multi sub first(Mu $test, *@values) { @values.first($test) }
multi sub first(Bool:D $t, *@v) { fail X::Match::Bool.new( type => 'first' ) }

proto sub first-index(|) {*}
multi sub first-index(Mu $test, @values) { @values.first-index($test) }
multi sub first-index(Mu $test, *@values) { @values.first-index($test) }
multi sub first-index(Bool:D $t,*@v) {
    fail X::Match::Bool.new(type => 'first-index');
}

proto sub last-index(|) {*}
multi sub last-index(Mu $test, @values) { @values.last-index($test) }
multi sub last-index(Mu $test, *@values) { @values.last-index($test) }
multi sub last-index(Bool:D $t, *@v) {
    fail X::Match::Bool.new(type => 'last-index');
}

proto sub join(|) { * }
multi sub join($sep = '', *@values) { @values.join($sep) }

proto sub pick(|) { * }
multi sub pick($n, @values) { @values.pick($n) }
multi sub pick($n, *@values) { @values.pick($n) }

proto sub roll(|) { * }
multi sub roll($n, @values) { @values.roll($n) }
multi sub roll($n, *@values) { @values.roll($n) }

proto sub keys(|) { * }
multi sub keys($x) { $x.keys }

proto sub values(|) { * }
multi sub values($x) { $x.values }

proto sub pairs(|) { * }
multi sub pairs($x) { $x.pairs }

proto sub kv(|) { * }
multi sub kv($x) { $x.kv }

proto sub elems(|) is nodal { * }
multi sub elems($a) is nodal { $a.elems }

proto sub end(|) { * }
multi sub end($a) { $a.end }

proto sub classify(|) { * }
multi sub classify( $test, *@items ) { Hash.^parameterize(Any,Any).new.classify-list( $test, @items ) }
#multi sub classify( $test, *@items, :$into! ) {   # problem in MMD
#    ( $into // $into.new).classify-list( $test, @items );
#}

proto sub categorize(|) { * }
multi sub categorize( $test, *@items ) { Hash.^parameterize(Any,Any).new.categorize-list( $test, @items ) }
#multi sub categorize( $test, *@items, :$into! ) {   # problem in MMD
#    ( $into // $into.new).categorize-list( $test, @items );
#}

proto sub uniq(|) { * }
multi sub uniq(*@values, |c) {
    DEPRECATED('unique', |<2014.12 2015.09>);
    @values.unique(|c)
}

proto sub unique(|) { * }
multi sub unique(*@values, |c) { @values.unique(|c) }

proto sub squish(|) { * }
multi sub squish(*@values, |c) { @values.squish(|c) }

proto sub sort(|) {*}
multi sub sort(*@values)      {
    nqp::istype(@values.AT-POS(0), Callable)
        ?? SEQ(my $cmp := @values.shift; @values.sort($cmp) )
        !! @values.sort;
}

proto sub item(|) is pure   { * }
multi sub item(\x)    { my $ = x }
multi sub item(*@a)   { my $ = nqp::p6parcel(nqp::p6argvmarray(), nqp::null()) }
multi sub item(Mu $a) { $a }

my $default= [];       # so that we can check missing parameters
sub RWPAIR(\k, \v) {   # internal fast pair creation
    my \p := nqp::create(Pair);
    nqp::bindattr(p, Enum, '$!key', k);
    nqp::bindattr(p, Enum, '$!value', v);
    p
}

sub OBJECT_HUH (\SELF) {
    my $huh := SELF.WHAT.perl;
    try { $huh ~= " {SELF.VAR.name}" };
    $huh;
}

sub SLICE_HUH ( \SELF, @nogo, Mu $d, %adv ) is hidden-from-backtrace {
    @nogo.unshift('delete')  # recover any :delete if necessary
      if @nogo && @nogo[0] ne 'delete' && %adv.EXISTS-KEY('delete');
    for <delete exists kv p k v> -> $valid { # check all valid params
        if nqp::existskey($d,nqp::unbox_s($valid)) {
            nqp::deletekey($d,nqp::unbox_s($valid));
            @nogo.push($valid);
        }
    }

    if nqp::elems($d) -> $elems {
        my @keys;
        my Mu $iter := nqp::iterator($d);
        @keys.push: nqp::p6box_s(nqp::iterkey_s(nqp::shift($iter))) while $iter;

        $elems > 1
          ?? fail "$elems unexpected named parameters (@keys.join(', ')) passed to {OBJECT_HUH(SELF)}"
          !! fail "Unexpected named parameter '@keys[0]' passed to {OBJECT_HUH(SELF)}";
    }

    else {
        fail "Unsupported combination of named parameters (@nogo.join(', ')) passed to {OBJECT_HUH(SELF)}";
    }
} #SLICE_HUH

sub DELETEKEY(Mu \d, str $key) {
    if nqp::existskey(d,$key) {
        my Mu $value := nqp::atkey(d,$key);
        nqp::deletekey(d,$key);
        $value;
    }
    else {
        Mu;
    }
} #DELETEKEY


sub dd(|) {
    my Mu $args := nqp::p6argvmarray();
    while $args {
        my $var  := nqp::shift($args);
        my $name := $var.VAR.?name;
        my $what := $var.?infinite
          ?? $var[^10].perl.chop ~ "...Inf)"
          !! $var.perl;
        note $name ?? "$name = $what" !! $what;
    }
    return
}
# vim: ft=perl6 expandtab sw=4
