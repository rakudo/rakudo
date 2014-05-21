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
    method exists (Any:U: $key) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :exists adverb");
        False;
    }
    method exists_key(Any:U: $key) { False }
    method exists_pos(Any:U: $pos) { False }
    method delete (Any:U: $key) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        Nil;
    }
    proto method delete_key(|) { * }
    multi method delete_key(Any:U: $key) { Nil }
    multi method delete_key(Any:D: $key) {
        fail "Can not remove values from a {self.^name}";
    }

    proto method delete_pos(|) { * }
    multi method delete_pos(Any:U: $pos) { Nil }
    multi method delete_pos(Any:D: $pos) {
        fail "Can not remove elements from a {self.^name}";
    }

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
    method rotor(|c) { self.list.rotor(|c) }
    method pick($n = 1) { self.list.pick($n) }
    method roll($n = 1) { self.list.roll($n) }
    method reverse() { self.list.reverse }
    method sort($by = &infix:<cmp>) { self.list.sort($by) }
    method values() { self.list }
    method keys()   { self.list.keys }
    method kv()     { self.list.kv }
    method pairs()  { self.list.pairs }
    method reduce(&with) { self.list.reduce(&with) }
    method combinations(|c) { self.list.combinations(|c) }
    method permutations(|c) { self.list.permutations(|c) }

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
    proto method map (|) { * }
    multi method map(Whatever) is rw { self }
    multi method map($block, :$label) is rw {
        MapIter.new(self, $block, Bool::True, :$label).list
    }
    method flatmap($block) is rw { flatmap($block, self) }
    method duckmap($block) is rw { duckmap($block, self) }
    method deepmap($block) is rw { deepmap($block, self) }

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
        SELF = Array.new;
        SELF.push(@values);
    }

    proto method unshift(|) { * }
    multi method unshift(Any:U \SELF: *@values) {
        SELF = Array.new;
        SELF.unshift(@values);
    }

    proto method grep(|) { * }
    multi method grep(Regex:D $test) is rw {
        self.map({ $_ if .match($test) });
    }
    multi method grep(Callable:D $test) is rw {
        self.map({ $_ if $test($_) });
    }
    multi method grep(Mu $test) is rw {
        self.map({ $_ if $_ ~~ $test });
    }

    proto method grep-index(|) { * }
    multi method grep-index(Regex:D $test) {
        my $index = -1;
        self.map: { $index++; +$index if .match($test) };
    }
    multi method grep-index(Callable:D $test) {
        my $index = -1;
        self.map: { $index++; +$index if $test($_) };
    }
    multi method grep-index(Mu $test) {
        my $index = -1;
        self.map: { $index++; +$index if $_ ~~ $test };
    }

    proto method first(|) { * }
    multi method first(Regex:D $test) is rw {
        self.map({ return $_ if .match($test) });
        Nil;
    }
    multi method first(Callable:D $test) is rw {
        self.map({ return $_ if $test($_) });
        Nil;
    }
    multi method first(Mu $test) is rw {
        self.map({ return $_ if $_ ~~ $test });
        Nil;
    }

    proto method first-index(|) { * }
    multi method first-index(Regex:D $test) {
        my $index = -1;
        self.map: { $index++; return $index if .match($test) };
        Nil;
    }
    multi method first-index(Callable:D $test) {
        my $index = -1;
        self.map: { $index++; return $index if $test($_) };
        Nil;
    }
    multi method first-index(Mu $test) {
        my $index = -1;
        self.map: { $index++; return $index if $_ ~~ $test };
        Nil;
    }

    proto method last-index(|) { * }
    multi method last-index(Regex:D $test) {
        my $index = self.elems;
        self.reverse.map: { $index--; return $index if .match($test) };
        Nil;
    }
    multi method last-index(Callable:D $test) {
        my $index = self.elems;
        self.reverse.map: { $index--; return $index if $test($_) };
        Nil;
    }
    multi method last-index(Mu $test) {
        my $index = self.elems;
        self.reverse.map: { $index--; return $index if $_ ~~ $test };
        Nil;
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
            -> { SELF.defined || (SELF = Array.new);
                 SELF.bind_pos($pos, $v) });
        $v
    }
    proto method assign_pos(|) { * }
    multi method assign_pos(\SELF: \pos, Mu \assignee) {
        SELF.at_pos(pos) = assignee;
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
            -> { SELF.defined || (SELF = Hash.new);
                 SELF.bind_key($key, $v) });
        $v
    }
    proto method bind_key(|) { * }
    multi method bind_key(Any:D: $key, $BIND ) {
        fail "postcircumfix:<\{ \}> binding not defined for type {self.WHAT.perl}";
    }
    multi method bind_key(Any:U \SELF: $key, $BIND ) is rw {
        SELF = Hash.new;
        SELF.bind_key($key, $BIND);
        $BIND
    }
    proto method assign_key(|) { * }
    multi method assign_key(\SELF: \key, Mu \assignee) {
        SELF.at_key(key) = assignee;
    }

    method FLATTENABLE_LIST() { 
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() { nqp::hash() }

    method Set()     {     Set.new-fp(self.list) }
    method SetHash() { SetHash.new-fp(self.list) }
    method Bag()     {     Bag.new-fp(self.list) }
    method BagHash() { BagHash.new-fp(self.list) }
    method Mix()     {     Mix.new-fp(self.list) }
    method MixHash() { MixHash.new-fp(self.list) }

    method KeySet() { DEPRECATED("'SetHash'"); self.SetHash }
    method KeyBag() { DEPRECATED("'BagHash'"); self.BagHash }

    method Supply() { self.list.Supply }
}
Metamodel::ClassHOW.exclude_parent(Any);

# builtin ops
proto infix:<===>($?, $?) is pure { * }
multi infix:<===>($?)    { Bool::True }
multi infix:<===>($a, $b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a.WHICH), nqp::unbox_s($b.WHICH)))
}

proto infix:<before>($, $?)  is pure { * }
multi infix:<before>($?)      { Bool::True }
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
# fails integration/99problems-21-to-30, test 12/13
#multi map(&code, @values) { @values.map(&code) }
multi map(&code, *@values) { @values.map(&code) }
multi map(Whatever, \a)    { a }
multi map(&code, Whatever) { (1..Inf).map(&code) }

proto grep(|) {*}
multi grep(Mu $test, @values) { @values.grep($test) }
multi grep(Mu $test, *@values) { @values.grep($test) }

proto grep-index(|) {*}
multi grep-index(Mu $test, @values) { @values.grep-index($test) }
multi grep-index(Mu $test, *@values) { @values.grep-index($test) }

proto first(|) {*}
multi first(Mu $test, @values) { @values.first($test) }
multi first(Mu $test, *@values) { @values.first($test) }

proto first-index(|) {*}
multi first-index(Mu $test, @values) { @values.first-index($test) }
multi first-index(Mu $test, *@values) { @values.first-index($test) }

proto last-index(|) {*}
multi last-index(Mu $test, @values) { @values.last-index($test) }
multi last-index(Mu $test, *@values) { @values.last-index($test) }

proto join(|) { * }
multi join($sep = '', *@values) { @values.join($sep) }

proto pick(|) { * }
multi pick($n, @values) { @values.pick($n) }
multi pick($n, *@values) { @values.pick($n) }

proto roll(|) { * }
multi roll($n, @values) { @values.roll($n) }
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
    nqp::istype(@values.at_pos(0), Callable)
        ?? (my $cmp := @values.shift; @values.sort($cmp) )
        !! @values.sort;
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

sub OBJECT_HUH (\SELF) {
    my $huh = SELF.WHAT.perl;
    try { $huh ~= " {SELF.VAR.name}" };
    $huh;
}

sub SLICE_HUH ( \SELF, @nogo, %a, %adv ) is hidden_from_backtrace {
    @nogo.unshift('delete')  # recover any :delete if necessary
      if @nogo && @nogo[0] ne 'delete' && %adv.exists_key('delete');
    @nogo.push( %a<delete exists kv p k v>:delete:k ); # all valid params

    if %a.elems {
        %a.elems > 1
              ?? fail "{%a.elems} unexpected named parameters ({%a.keys.join(', ')}) passed to {OBJECT_HUH(SELF)}"
          !! fail "Unexpected named parameter '{%a.keys}' passed to {OBJECT_HUH(SELF)}";
    }

    else {
        fail "Unsupported combination of named parameters ({@nogo.join(', ')}) passed to {OBJECT_HUH(SELF)}";
    }
} #SLICE_HUH

# internal 1 element hash/array access with adverbs
sub SLICE_ONE ( \SELF, $one, $array, *%adv ) is hidden_from_backtrace {
    fail "Cannot use negative index $one on {SELF.WHAT.perl}"
      if $array && $one < 0;

    my $ex = SELF.can( $array ?? 'exists_pos' !! 'exists_key' )[0];

    my %a = %adv.clone;
    my @nogo;

    my \result = do {

        if %a.delete_key('delete') {          # :delete:*
            my $de = SELF.can( $array ?? 'delete_pos' !! 'delete_key' )[0];
            if %a.delete_key('SINK') {          # :delete:SINK
                $de(SELF,$one);
                Nil;
            }
            elsif !%a {                         # :delete
                $de(SELF,$one);
            }
            elsif %a.exists_key('exists') {     # :delete:exists(0|1):*
                my $exists   := %a.delete_key('exists');
                my $wasthere := $ex(SELF,$one);
                $de(SELF,$one);
                if !%a {                          # :delete:exists(0|1)
                    !( $wasthere ?^ $exists )
                }
                elsif %a.exists_key('kv') {       # :delete:exists(0|1):kv(0|1)
                    my $kv := %a.delete_key('kv');
                    if !%a {
                        !$kv | $wasthere
                          ?? ( $one, !( $wasthere ?^ $exists ) ) 
                          !! ();
                    }
                    else {
                        @nogo = <delete exists kv>;
                    }
                }
                elsif %a.exists_key('p') {        # :delete:exists(0|1):p(0|1)
                    my $p := %a.delete_key('p');
                    if !%a {
                        !$p | $wasthere
                          ?? RWPAIR($one, !($wasthere ?^ $exists) )
                          !! ();
                    }
                    else {
                        @nogo = <delete exists p>;
                    }
                }
                else {
                    @nogo = <delete exists>;
                }
            }
            elsif %a.exists_key('kv') {         # :delete:kv(0|1)
                my $kv := %a.delete_key('kv');
                if !%a {
                    !$kv | $ex(SELF,$one)
                      ?? ( $one, $de(SELF,$one) )
                      !! ();
                }
                else {
                    @nogo = <delete kv>;
                }
            }
            elsif %a.exists_key('p') {          # :delete:p(0|1)
                my $p := %a.delete_key('p');
                if !%a {
                    !$p | $ex(SELF,$one)
                      ?? RWPAIR($one, $de(SELF,$one))
                      !! ();
                }
                else {
                    @nogo = <delete p>;
                }
            }
            elsif %a.exists_key('k') {          # :delete:k(0|1)
                my $k := %a.delete_key('k');
                if !%a {
                    !$k | $ex(SELF,$one)
                      ?? do { $de(SELF,$one); $one }
                      !! ();
                }
                else {
                    @nogo = <delete k>;
                }
            }
            elsif %a.exists_key('v') {          # :delete:v(0|1)
                my $v := %a.delete_key('v');
                if !%a {
                    !$v | $ex(SELF,$one)
                      ?? $de(SELF,$one)
                      !! ();
                }
                else {
                    @nogo = <delete v>;
                }
            }
            else {
                @nogo = <delete>;
            }
        }
        elsif %a.exists_key('exists') {       # :!delete?:exists(0|1):*
            my $exists  := %a.delete_key('exists');
            my $wasthere = $ex(SELF,$one);
            if !%a {                            # :!delete?:exists(0|1)
                !( $wasthere ?^ $exists )
            }
            elsif %a.exists_key('kv') {         # :!delete?:exists(0|1):kv(0|1)
                my $kv := %a.delete_key('kv');
                if !%a {
                    !$kv | $wasthere
                      ?? ( $one, !( $wasthere ?^ $exists ) )
                      !! ();
                }
                else {
                    @nogo = <exists kv>;
                }
            }
            elsif %a.exists_key('p') {          # :!delete?:exists(0|1):p(0|1)
                my $p := %a.delete_key('p');
                if !%a {
                    !$p | $wasthere
                      ?? RWPAIR($one, !( $wasthere ?^ $exists ))
                      !! ();
                }
                else {
                    @nogo = <exists p>;
                }
            }
            else {
                @nogo = <exists>;
            }
        }
        elsif %a.exists_key('kv') {           # :!delete?:kv(0|1):*
            my $kv := %a.delete_key('kv');
            if !%a {                            # :!delete?:kv(0|1)
                !$kv | $ex(SELF,$one)
                  ?? ($one, $array ?? SELF.at_pos($one) !! SELF.at_key($one))
                  !! ();
            }
            else {
                @nogo = <kv>;
            }
        }
        elsif %a.exists_key('p') {            # :!delete?:p(0|1):*
            my $p := %a.delete_key('p');
            if !%a {                            # :!delete?:p(0|1)
                !$p | $ex(SELF,$one)
                  ?? RWPAIR($one,
                       $array ?? SELF.at_pos($one) !! SELF.at_key($one))
                  !! ();
            }
            else {
                @nogo = <p>;
            }
        }
        elsif %a.exists_key('k') {            # :!delete?:k(0|1):*
            my $k := %a.delete_key('k');
            if !%a {                            # :!delete?:k(0|1)
                !$k | $ex(SELF,$one)
                  ?? $one
                  !! ();
            }
            else {
                @nogo = <k>;
            }
        }
        elsif %a.exists_key('v') {            # :!delete?:v(0|1):*
            my $v := %a.delete_key('v');            # :!delete?:v(0|1)
            if !%a {
                !$v | $ex(SELF,$one)
                  ?? ($array ?? SELF.at_pos($one) !! SELF.at_key($one))
                  !! ();
            }
            else {
                @nogo = <v>;
            }
        }
        elsif !%a {                           # :!delete
            $array ?? SELF.at_pos($one) !! SELF.at_key($one);
        }
    };

    @nogo || %a
      ?? SLICE_HUH( SELF, @nogo, %a, %adv )
      !! result;
} #SLICE_ONE

# internal >1 element hash/array access with adverbs
sub SLICE_MORE ( \SELF, $more, $array, *%adv ) is hidden_from_backtrace {
    my %a = %adv.clone;
    my @nogo;

    my $at = SELF.can( $array ?? 'at_pos'     !! 'at_key'     )[0];
    my $ex = SELF.can( $array ?? 'exists_pos' !! 'exists_key' )[0];

    my \result = do {  

        if %a.delete_key('delete') {       # :delete:*
            my $de = SELF.can( $array ?? 'delete_pos' !! 'delete_key' )[0];
            if %a.delete_key('SINK') {       # :delete:SINK
                $de(SELF,$_) for $more;
                Nil;
            }
            elsif !%a {                      # :delete
                $more.list.map( { $de(SELF,$_) } ).eager.Parcel;
            }
            elsif %a.exists_key('exists') {  # :delete:exists(0|1):*
                my $exists := %a.delete_key('exists');
                my $wasthere; # no need to initialize every iteration of map
                if !%a {                       # :delete:exists(0|1)
                    $more.list.map( {
                        $de(SELF,$_) if $wasthere = $ex(SELF,$_);
                        !( $wasthere ?^ $exists );
                    } ).eager.Parcel
                }
                elsif %a.exists_key('kv') {    # :delete:exists(0|1):kv(0|1):*
                    my $kv := %a.delete_key('kv');
                    if !%a {                     # :delete:exists(0|1):kv(0|1)
                        $more.list.map( {
                            $de(SELF,$_) if $wasthere = $ex(SELF,$_);
                            ($_, !( $wasthere ?^ $exists )) 
                              if !$kv | $wasthere;
                        } ).eager.Parcel
                    }
                    else {
                        @nogo = <delete exists kv>;
                    }
                }
                elsif %a.exists_key('p') {     # :delete:exists(0|1):p(0|1):*
                    my $p := %a.delete_key('p');
                    if !%a {                     # :delete:exists(0|1):p(0|1)
                        $more.list.map( {
                            $de(SELF,$_) if $wasthere = $ex(SELF,$_);
                            RWPAIR($_,!($wasthere ?^ $exists))
                              if !$p | $wasthere;
                        } ).eager.Parcel
                    }
                    else {
                        @nogo = <delete exists p>;
                    }
                }
                else {
                    @nogo = <delete exists>;
                }
            }
            elsif %a.exists_key('kv') {      # :delete:kv(0|1):*
                my $kv := %a.delete_key('kv');
                if !%a {                       # :delete:kv(0|1)
                    $kv
                      ?? $more.list.map( {
                             ( $_, $de(SELF,$_) ) if $ex(SELF,$_);
                         } ).eager.Parcel
                      !! $more.list.map( {
                             ( $_, $de(SELF,$_) )
                         } ).eager.Parcel;
                }
                else {
                    @nogo = <delete kv>;
                }
            }
            elsif %a.exists_key('p') {       # :delete:p(0|1):*
                my $p := %a.delete_key('p');
                if !%a {                       # :delete:p(0|1)
                    $p
                      ?? $more.list.map( {
                             RWPAIR($_, $de(SELF,$_)) if $ex(SELF,$_);
                         } ).eager.Parcel
                      !! $more.list.map( {
                             RWPAIR($_, $de(SELF,$_))
                         } ).eager.Parcel;
                }
                else {
                    @nogo = <delete p>;
                }
            }
            elsif %a.exists_key('k') {       # :delete:k(0|1):*
                my $k := %a.delete_key('k');
                if !%a {                       # :delete:k(0|1)
                    $k
                      ?? $more.list.map( {
                             ( $de(SELF,$_); $_ ) if $ex(SELF,$_);
                         } ).eager.Parcel
                      !! $more.list.map( {
                             $de(SELF,$_); $_
                         } ).eager.Parcel;
                }
                else {
                    @nogo = <delete k>;
                }
            }
            elsif %a.exists_key('v') {       # :delete:v(0|1):*
                my $v := %a.delete_key('v');
                if !%a {                       # :delete:v(0|1)
                    $v
                      ?? $more.list.map( {
                             $de(SELF,$_) if $ex(SELF,$_);
                     } ).eager.Parcel
                      !! $more.list.map( {
                             $de(SELF,$_)
                     } ).eager.Parcel;
                }
                else {
                    @nogo = <delete v>;
                }
            }
            else {
                @nogo = <delete>;
            }
        }
        elsif %a.exists_key('exists') {    # :!delete?:exists(0|1):*
            my $exists := %a.delete_key('exists');
            if !%a {                         # :!delete?:exists(0|1)
                $more.list.map({ !( $ex(SELF,$_) ?^ $exists ) }).eager.Parcel;
            }
            elsif %a.exists_key('kv') {      # :!delete?:exists(0|1):kv(0|1):*
                my $kv := %a.delete_key('kv');
                if !%a {                       # :!delete?:exists(0|1):kv(0|1)
                    $kv
                      ?? $more.list.map( {
                             ( $_, $exists ) if $ex(SELF,$_);
                         } ).eager.Parcel
                      !! $more.list.map( {
                             ( $_, !( $ex(SELF,$_) ?^ $exists ) )
                         } ).eager.Parcel;
                }
                else {
                    @nogo = <exists kv>;
                }
            }
            elsif %a.exists_key('p') {       # :!delete?:exists(0|1):p(0|1):*
                my $p := %a.delete_key('p');
                if !%a {                       # :!delete?:exists(0|1):p(0|1)
                    $p
                      ?? $more.list.map( {
                             RWPAIR( $_, $exists ) if $ex(SELF,$_);
                         } ).eager.Parcel
                      !! $more.list.map( {
                             RWPAIR( $_, !( $ex(SELF,$_) ?^ $exists ) )
                         } ).eager.Parcel;
                }
                else {
                    @nogo = <exists p>;
                }
            }
            else {
                @nogo = <exists>;
            }
        }
        elsif %a.exists_key('kv') {        # :!delete?:kv(0|1):*
            my $kv := %a.delete_key('kv');
            if !%a {                         # :!delete?:kv(0|1)
                $kv
                  ?? $more.list.map( {
                         ($_, $at(SELF,$_)) if $ex(SELF,$_);
                     } ).eager.Parcel
                  !! $more.list.map( {
                         ($_, $at(SELF,$_))
                     } ).eager.Parcel;
            }
            else {
                @nogo = <kv>;
            }
        }
        elsif %a.exists_key('p') {         # :!delete?:p(0|1):*
            my $p := %a.delete_key('p');
            if !%a {                         # :!delete?:p(0|1)
                $p
                  ?? $more.list.map( {
                         RWPAIR($_, $at(SELF,$_)) if $ex(SELF,$_);
                     } ).eager.Parcel
                  !! $more.list.map( {
                         RWPAIR( $_, $at(SELF,$_) )
                     } ).eager.Parcel;
            }
            else {
                @nogo = <p>
            }
        }
        elsif %a.exists_key('k') {         # :!delete?:k(0|1):*
            my $k := %a.delete_key('k');
            if !%a {                         # :!delete?:k(0|1)
                $k
                  ?? $more.list.map( { $_ if $ex(SELF,$_) } ).eager.Parcel
                  !! $more.list.eager.Parcel;
            }
            else {
                @nogo = <k>;
            }
        }
        elsif %a.exists_key('v') {         # :!delete?:v(0|1):*
            my $v := %a.delete_key('v');
            if !%a {                         # :!delete?:v(0|1)
                $v
                  ??  $more.list.map( {
                          $at(SELF,$_) if $ex(SELF,$_);
                      } ).eager.Parcel
                  !!  $more.list.map( {
                          $at(SELF,$_)
                      } ).eager.Parcel;
            }
            else {
                @nogo = <v>;
            }
        }
        elsif !%a {                        # :!delete
            $more.list.map( { $at(SELF,$_) } ).eager.Parcel;
        }
    }

    @nogo || %a
      ?? SLICE_HUH( SELF, @nogo, %a, %adv )
      !! result;
} #SLICE_MORE

# vim: ft=perl6 expandtab sw=4
