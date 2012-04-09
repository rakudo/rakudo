my class MapIter { ... }
my class Whatever { ... }
my class Range { ... }
my class X::Bind::ZenSlice { ... }

my class Any {
    multi method ACCEPTS(Any:D: Mu \$a) { self === $a }

    ########
    # List-like methods for Any.
    ########

    method eager() { nqp::p6list(nqp::list(self), List, Bool::True).eager }
    method elems() { self.list.elems }
    method end()   { self.list.end }
    method classify(&t) { self.list.classify(&t) }
    method uniq() { self.list.uniq }
    method infinite() { Mu }
    method flat() { nqp::p6list(nqp::list(self), List, Bool::True) }
    method tree(*@a) { self.list.tree(|@a) }
    method hash() { my %h = self }
    method list() { nqp::p6list(nqp::list(self), List, Mu) }
    method pick($n = 1) { self.list.pick($n) }
    method roll($n = 1) { self.list.roll($n) }
    method reverse() { self.list.reverse }
    method sort($by = &infix:<cmp>) { self.list.sort($by) }
    method values() { self.list }
    method keys()   { self.list.keys }
    method kv()     { self.list.kv }
    method pairs()  { self.list.pairs }

    method Array() { Array.new(self.flat) }

    method grep(Mu $test) is rw {
        self.map({ $_ if $_ ~~ $test });
    }
    method first(Mu $test) is rw {
        for self.list {
            return $_ if $test.ACCEPTS($_);
        }
        fail 'No values matched';
    }

    method join($separator = '') {
        my $list = (self,).flat.eager;
        my Mu $rsa := pir::new__Ps('ResizableStringArray');
        $list.gimme(4);        # force reification of at least 4 elements
        nqp::push_s($rsa, nqp::unbox_s($list.shift.Stringy)) 
            while $list.gimme(0);
        nqp::push_s($rsa, '...') if $list.infinite;
        nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Stringy), $rsa))
    }

    method map($block) is rw {
        MapIter.new(:list((self,).flat), :block($block)).list
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

    proto method postcircumfix:<[ ]>(|$) { * }
    multi method postcircumfix:<[ ]>() { self.list }
    multi method postcircumfix:<[ ]>(:$BIND!) { die(X::Bind::ZenSlice.new()) }
    multi method postcircumfix:<[ ]>($pos) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.at_pos($pos)
    }
    multi method postcircumfix:<[ ]>($pos, :$BIND! is parcel) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.bind_pos($pos, $BIND)
    }
    multi method postcircumfix:<[ ]>(int $pos) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.at_pos($pos)
    }
    multi method postcircumfix:<[ ]>(int $pos, :$BIND! is parcel) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.bind_pos($pos, $BIND)
    }
    multi method postcircumfix:<[ ]>(Positional \$pos) is rw {
        if nqp::iscont($pos) {
            fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
            return self.at_pos($pos)
        }
        my $list = $pos.flat;
        $list.gimme(*);
        $list.map($list.infinite
                   ?? { last if $_ >= self.list.gimme($_ + 1); self[$_] }
                   !! { self[$_] }).eager.Parcel;
    }
    multi method postcircumfix:<[ ]>(Positional $pos, :$BIND!) is rw {
        die "Cannot bind to an array slice"
    }
    multi method postcircumfix:<[ ]>(Callable $block) is rw {
        self[$block(|(self.elems xx $block.count))]
    }
    multi method postcircumfix:<[ ]>(Callable $block, :$BIND!) is rw {
        die "Cannot bind to a callable array slice"; # WhateverCode?
    }
    multi method postcircumfix:<[ ]>(Whatever) is rw {
        self[^self.elems]
    }
    multi method postcircumfix:<[ ]>(Whatever, :$BIND!) is rw {
        die "Cannot bind to a whatever array slice"
    }

    method at_pos($pos) is rw {
        if self.defined {
            fail X::OutOfRange.new(
                what => 'Index',
                got  => $pos,
                range => (0..0)
            ) if $pos != 0;
            return self;
        }
    }
    
    method all() { all(self.list) }
    method any() { any(self.list) }
    method one() { one(self.list) }
    method none() { none(self.list) }

    ########
    # Hash-like methods for Any.
    ########
    proto method postcircumfix:<{ }>(|$) { * }
    multi method postcircumfix:<{ }>() { self }
    multi method postcircumfix:<{ }>(:$BIND!) { die(X::Bind::ZenSlice.new(:what<hash>)) }
    multi method postcircumfix:<{ }>($key) is rw {
        self.at_key($key)
    }
    multi method postcircumfix:<{ }>($key, :$BIND! is parcel) is rw {
        self.bind_key($key, $BIND)
    }
    multi method postcircumfix:<{ }>(Positional \$key) is rw {
        nqp::iscont($key) 
          ?? self.at_key($key) 
          !! $key.map({ self{$_} }).eager.Parcel
    }
    multi method postcircumfix:<{ }>(Positional $key, :$BIND!) is rw {
        die "Cannot bind to a hash slice"
    }
    multi method postcircumfix:<{ }>(Whatever) is rw {
        self{self.keys}
    }
    multi method postcircumfix:<{ }>(Whatever, :$BIND!) is rw {
        die "Cannot bind to a whatever hash slice"
    }

    method reduce(&with) { self.list.reduce(&with) }
}
Metamodel::ClassHOW.exclude_parent(Any);

proto infix:<===>($?, $?) { * }
multi infix:<===>($a?)    { Bool::True }
multi infix:<===>($a, $b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a.WHICH), nqp::unbox_s($b.WHICH)))
}

proto infix:<cmp>($, $) { * }
multi infix:<cmp>(\$a, \$b) { 
    return -1 if $a == -$Inf || $b == $Inf;
    return  1 if $a ==  $Inf || $b == -$Inf;
    $a.Stringy cmp $b.Stringy 
}

proto infix:<before>(|$)       { * }
multi infix:<before>($x?)      { Bool::True }
multi infix:<before>(\$a, \$b) { ($a cmp $b) < 0 }

proto infix:<after>(|$)        { * }
multi infix:<after>($x?)       { Bool::True }
multi infix:<after>(\$a, \$b)  { ($a cmp $b) > 0 }

# XXX: should really be '$a is rw' (no \) in the next four operators
proto prefix:<++>(|$)             { * }
multi prefix:<++>(Mu:D \$a is rw) { $a = $a.succ }
multi prefix:<++>(Mu:U \$a is rw) { $a = 1 }
proto prefix:<-->(|$)             { * }
multi prefix:<-->(Mu:D \$a is rw) { $a = $a.pred }
multi prefix:<-->(Mu:U \$a is rw) { $a = -1 }

proto postfix:<++>(|$)             { * }
multi postfix:<++>(Mu:D \$a is rw) { my $b = $a; $a = $a.succ; $b }
multi postfix:<++>(Mu:U \$a is rw) { $a = 1; 0 }
proto postfix:<-->(|$)             { * }
multi postfix:<-->(Mu:D \$a is rw) { my $b = $a; $a = $a.pred; $b }
multi postfix:<-->(Mu:U \$a is rw) { $a = -1; 0 }

proto infix:<min>(|$)     { * }
multi infix:<min>(*@args) { @args.min }
# XXX the multi version suffers from a multi dispatch bug
# where the mandatory named is ignored in the presence of a slurpy
#proto sub min(|$)     { * }
#multi sub min(*@args) { @args.min() }
#multi sub min(*@args, :&by!) { @args.min(&by) }
sub min(*@args, :&by = &infix:<cmp>) { @args.min(&by) }


proto infix:<max>(|$)     { * }
multi infix:<max>(*@args) { @args.max }
#proto sub max(|$) { * }
#multi sub max(*@args) { @args.max() }
#multi sub max(*@args, :&by!) { @args.max(&by) }
sub max(*@args, :&by = &infix:<cmp>) { @args.max(&by) }

proto infix:<minmax>(|$)     { * }
multi infix:<minmax>(*@args) { @args.minmax }
#proto sub minmax(|$) { * }
#multi sub minmax(*@args) { @args.minmax() }
#multi sub minmax(*@args, :&by!) { @args.minmax(&by) }
sub minmax(*@args, :&by = &infix:<cmp>) { @args.minmax(&by) }

proto map(|$) {*}
multi map(&code, *@values) { @values.map(&code) }

proto grep(|$) {*}
multi grep(Mu $test, *@values) { @values.grep($test) }

proto first(|$) {*}
multi first(Mu $test, *@values) { @values.first($test) }

proto join(|$) { * }
multi join($sep = '', *@values) { @values.join($sep) }

proto pick(|$) { * }
multi pick($n, *@values) { @values.pick($n) }

proto roll(|$) { * }
multi roll($n, *@values) { @values.roll($n) }

proto keys(|$) { * }
multi keys($x) { $x.keys }

proto values(|$) { * }
multi values($x) { $x.values }

proto pairs(|$) { * }
multi pairs($x) { $x.pairs }

proto kv(|$) { * }
multi kv($x) { $x.kv }

proto elems(|$) { * }
multi elems($a) { $a.elems }

proto end(|$) { * }
multi end($a) { $a.end }

proto classify(|$) { * }
multi classify(&test, *@items) { @items.classify(&test) }

proto uniq(|$) { * }
multi uniq(*@values) { @values.uniq }

proto sub sort(|$) {*}
multi sub sort(*@values)      {
    @values.at_pos(0).^does(Callable)
        ?? do { my $cmp := @values.shift; @values.sort($cmp) }
        !!  @values.sort;
}

multi sub item(*@a) { my $ = @a }
multi sub item(Mu $a) { $a }
