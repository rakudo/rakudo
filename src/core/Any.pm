my class MapIter { ... }
my class Pair { ... }
my class Range { ... }
my class X::Bind::Slice { ... }
my class X::Bind::ZenSlice { ... }

my class Any { # declared in BOOTSTRAP
    # my class Any is Mu {

    multi method ACCEPTS(Any:D: Mu \a) { self === a }

    # primitives
    method infinite()   { Nil }
    method exists(Any:U: $key) { False }
    method delete(Any:U: $key) { Nil }
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
    proto method bind_key(|) { * }
    multi method bind_key(Any:D: $key, $BIND ) {
        fail "postcircumfix:<\{ \}> binding not defined for type {self.WHAT.perl}";
    }
    multi method bind_key(Any:U \SELF: $key, $BIND ) is rw {
        &infix:<=>(SELF, Hash.new);
        SELF.bind_key($key, $BIND);
        $BIND
    }

    method FLATTENABLE_LIST() { 
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() { nqp::hash() }

    method Set()    {    Set.new-fp(self.list) }
    method KeySet() { KeySet.new-fp(self.list) }
    method Bag()    {    Bag.new-fp(self.list) }
    method KeyBag() { KeyBag.new-fp(self.list) }
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

my $default= [];       # so that we can check missing parameters
sub RWPAIR(\k, \v) {   # internal fast pair creation
    my \p := nqp::create(Pair);
    nqp::bindattr(p, Enum, '$!key', k);
    nqp::bindattr(p, Enum, '$!value', v);
    p
}
