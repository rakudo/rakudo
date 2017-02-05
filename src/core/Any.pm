my class Pair                   { ... }
my class Range                  { ... }
my class X::Adverb              { ... }
my class X::Bind                { ... }
my class X::Bind::Slice         { ... }
my class X::Bind::ZenSlice      { ... }
my class X::Item                { ... }
my class X::Match::Bool         { ... }
my class X::Pairup::OddNumber   { ... }
my class X::Subscript::Negative { ... }

my role  Numeric { ... }

my class Any { # declared in BOOTSTRAP
    # my class Any is Mu

    multi method ACCEPTS(Any:D: Mu:D \a) { self === a }
    multi method ACCEPTS(Any:D: Mu:U \a) { False }
    multi method ACCEPTS(Any:U: Any \topic) { # use of Any on topic to force autothreading
        nqp::p6bool(nqp::istype(topic, self)) # so that all(@foo) ~~ Type works as expected
    }

    proto method EXISTS-KEY(|) is nodal { * }
    multi method EXISTS-KEY(Any:U: $) { False }
    multi method EXISTS-KEY(Any:D: $) { False }

    proto method DELETE-KEY(|) is nodal { * }
    multi method DELETE-KEY(Any:U: $) { Nil }
    multi method DELETE-KEY(Any:D: $) {
        Failure.new("Can not remove values from a {self.^name}")
    }

    proto method DELETE-POS(|) is nodal { * }
    multi method DELETE-POS(Any:U: $pos) { Nil }
    multi method DELETE-POS(Any:D: $pos) {
        Failure.new("Can not remove elements from a {self.^name}")
    }
    multi method DELETE-POS(Any:D: \one, \two) is raw {
        self.AT-POS(one).DELETE-POS(two)
    }
    multi method DELETE-POS(Any:D: \one, \two, \three) is raw {
        self.AT-POS(one).AT-POS(two).DELETE-POS(three)
    }
    multi method DELETE-POS(Any:D: **@indices) {
        my $final := @indices.pop;
        Rakudo::Internals.WALK-AT-POS(self,@indices).DELETE-POS($final)
    }

    method cache() { self.list }

    proto method list(|) is nodal { * }
    multi method list(Any:U:) { infix:<,>(self) }
    multi method list(Any:D \SELF:) { infix:<,>(SELF) }

    proto method flat(|) is nodal { * }
    multi method flat() { self.list.flat }

    proto method eager(|) is nodal { * }
    multi method eager() { self.list.eager }

    # derived from .list
    proto method List(|) is nodal { * }
    multi method List() { self.list }
    proto method Slip(|) is nodal { * }
    multi method Slip() { self.list.Slip }
    proto method Array(|) is nodal { * }
    multi method Array() { self.list.Array }

    proto method hash(|) is nodal { * }
    multi method hash(Any:U:) { my % = () }
    multi method hash(Any:D:) { my % = self }

    # derived from .hash
    proto method Hash(|) is nodal { * }
    multi method Hash() { self.hash.Hash }

    proto method Map(|) is nodal { * }
    multi method Map() { self.hash.Map }

    proto method elems(|) is nodal { * }
    multi method elems(Any:U:) { 1 }
    multi method elems(Any:D:) { self.list.elems }

    proto method end(|) is nodal { * }
    multi method end(Any:U:) { 0 }
    multi method end(Any:D:) { self.list.end }

    proto method keys(|) is nodal { * }
    multi method keys(Any:U:) { () }
    multi method keys(Any:D:) { self.list.keys }

    proto method kv(|) is nodal { * }
    multi method kv(Any:U:) { () }
    multi method kv(Any:D:) { self.list.kv }

    proto method values(|) is nodal { * }
    multi method values(Any:U:) { () }
    multi method values(Any:D:) { self.list }

    proto method pairs(|) is nodal { * }
    multi method pairs(Any:U:) { () }
    multi method pairs(Any:D:) { self.list.pairs }

    proto method antipairs(|) is nodal { * }
    multi method antipairs(Any:U:) { () }
    multi method antipairs(Any:D:) { self.list.antipairs }

    proto method invert(|) is nodal { * }

    proto method pick(|) is nodal { * }
    multi method pick()   { self.list.pick     }
    multi method pick($n) { self.list.pick($n) }

    proto method roll(|) is nodal { * }
    multi method roll()   { self.list.roll     }
    multi method roll($n) { self.list.roll($n) }

    proto method iterator(|) { * }
    multi method iterator(Any:) { self.list.iterator }

    proto method match(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method match(Any:U: |) { self.Str; nqp::getlexcaller('$/') = Nil }

    proto method classify(|) is nodal { * }
    multi method classify() {
        die "Must specify something to classify with, a Callable, Hash or List";
    }
    multi method classify(Whatever) {
        die "Doesn't make sense to classify with itself";
    }
    multi method classify($test, :$into!, :&as)   {
        ( $into // $into.new ).classify-list( $test, self, :&as);
    }
    multi method classify($test, :&as)   {
        Hash.^parameterize(Any,Any).new.classify-list( $test, self, :&as );
    }

    proto method categorize(|) is nodal { * }
    multi method categorize() {
        die "Must specify something to categorize with, a Callable, Hash or List";
    }
    multi method categorize(Whatever) {
        die "Doesn't make sense to categorize with itself";
    }
    multi method categorize($test, :$into!, :&as) {
        ( $into // $into.new ).categorize-list( $test, self.list, :&as );
    }
    multi method categorize($test, :&as) {
        Hash.^parameterize(Any,Any).new.categorize-list($test, self.list, :&as);
    }

    method reverse() is nodal { self.list.reverse }
    method combinations(|c) is nodal { self.list.combinations(|c) }
    method permutations(|c) is nodal { self.list.permutations(|c) }
    method join($separator = '') is nodal { self.list.join($separator) }

    # XXX GLR should move these
    method nodemap(&block) is nodal { nodemap(&block, self) }
    method duckmap(&block) is nodal { duckmap(&block, self) }
    method deepmap(&block) is nodal { deepmap(&block, self) }

    # XXX GLR Do we need tree post-GLR?
    proto method tree(|) is nodal { * }
    multi method tree(Any:U:) { self }
    multi method tree(Any:D:) {
        nqp::istype(self, Iterable)
            ?? self.map({ .tree }).item
            !! self
    }
    multi method tree(Any:D: Whatever ) { self.tree }
    multi method tree(Any:D: Int(Cool) $count) {
        nqp::istype(self, Iterable) && $count > 0
            ?? self.map({ .tree($count - 1) }).item
            !! self
    }
    multi method tree(Any:D: @ [&first, *@rest]) { self.tree(&first, |@rest); }
    multi method tree(Any:D: &first, *@rest) {
        nqp::istype(self, Iterable)
            ?? @rest ?? first(self.map({ .tree(|@rest) }))
                     !! first(self)
            !! self
    }

    # auto-vivifying
    proto method push(|) is nodal {*}
    multi method push(Any:U \SELF: |values) {
        SELF = nqp::istype(SELF,Positional) ?? SELF.new !! Array.new;
        SELF.push(|values);
    }

    proto method append(|) is nodal { * }
    multi method append(Any:U \SELF: |values) {
        SELF = nqp::istype(SELF,Positional) ?? SELF.new !! Array.new;
        SELF.append(|values);
    }

    proto method unshift(|) is nodal { * }
    multi method unshift(Any:U \SELF: |values) {
        SELF = Array.new;
        SELF.unshift(|values);
    }

    proto method prepend(|) is nodal { * }
    multi method prepend(Any:U \SELF: |values) {
        SELF = Array.new;
        SELF.prepend(|values);
    }

    proto method EXISTS-POS(|) is nodal { * }
    multi method EXISTS-POS(Any:U: Any:D $) { False }
    multi method EXISTS-POS(Any:U: Any:U $pos) {
        die "Cannot use '{$pos.^name}' as an index";
    }

    multi method EXISTS-POS(Any:D: int \pos) {
        nqp::p6bool(nqp::iseq_i(pos,0));
    }
    multi method EXISTS-POS(Any:D: Int:D \pos) {
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Num:D \pos) {
        X::Item.new(aggregate => self, index => pos).throw
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int));
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Any:D \pos) {
        pos.Int == 0;
    }
    multi method EXISTS-POS(Any:D: Any:U \pos) {
        die "Cannot use '{pos.^name}' as an index";
    }
    multi method EXISTS-POS(Any:D: \one, \two) is raw {
        self.AT-POS(one).EXISTS-POS(two)
    }
    multi method EXISTS-POS(Any:D: \one, \two,\three) is raw {
        self.AT-POS(one).AT-POS(two).EXISTS-POS(three)
    }
    multi method EXISTS-POS(Any:D: **@indices) {
        my $final := @indices.pop;
        Rakudo::Internals.WALK-AT-POS(self,@indices).EXISTS-POS($final)
    }

    proto method AT-POS(|) is nodal {*}
    multi method AT-POS(Any:U \SELF: int \pos) is raw {
        nqp::p6bindattrinvres(
          my $scalar,
          Scalar,
          '$!whence',
          -> { nqp::if(
                 nqp::isconcrete(SELF),
                 SELF,
                 (SELF = Array.new)
               ).BIND-POS(pos, $scalar)
             }
        )
    }
    multi method AT-POS(Any:U \SELF: Int:D \pos) is raw {
        nqp::p6bindattrinvres(
          my $scalar,
          Scalar,
          '$!whence',
          -> { nqp::if(
                 nqp::isconcrete(SELF),
                 SELF,
                 (SELF = Array.new)
               ).BIND-POS(pos, $scalar)
             }
        )
    }
    multi method AT-POS(Any:U: Num:D \pos) is raw {
        nqp::isnanorinf(pos)
          ?? Failure.new(X::Item.new(aggregate => self, index => pos))
          !! self.AT-POS(nqp::unbox_i(pos.Int))
    }
    multi method AT-POS(Any:U: Any:D \pos) is raw {
        self.AT-POS(nqp::unbox_i(pos.Int));
    }

    multi method AT-POS(Any:D: int \pos) is raw {
        pos
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'), :got(pos), :range<0..0>))
          !! self
    }
    multi method AT-POS(Any:D: Int:D \pos) is raw {
        pos
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'), :got(pos), :range<0..0>))
          !! self
    }
    multi method AT-POS(Any:D: Num:D \pos) is raw {
        nqp::isnanorinf(pos)
          ?? Failure.new(X::Item.new(aggregate => self, index => pos))
          !! self.AT-POS(nqp::unbox_i(pos.Int))
    }
    multi method AT-POS(Any:D: Any:D \pos) is raw {
        self.AT-POS(nqp::unbox_i(pos.Int));
    }
    multi method AT-POS(Any:   Any:U \pos) is raw {
        die "Cannot use '{pos.^name}' as an index";
    }
    multi method AT-POS(Any:D: \one, \two) is raw {
        self.AT-POS(one).AT-POS(two)
    }
    multi method AT-POS(Any:D: \one, \two, \three) is raw {
        self.AT-POS(one).AT-POS(two).AT-POS(three)
    }
    multi method AT-POS(Any:D: **@indices) is raw {
        my $final := @indices.pop;
        Rakudo::Internals.WALK-AT-POS(self,@indices).AT-POS($final)
    }

    proto method ZEN-POS(|) { * }
    multi method ZEN-POS(*%unexpected) {
        %unexpected
          ?? Failure.new(X::Adverb.new(
               :what('[] slice'),
               :source(try { self.VAR.name } // self.WHAT.perl),
               :unexpected(%unexpected.keys)))
          !! self
    }

    proto method ZEN-KEY(|) { * }
    multi method ZEN-KEY(*%unexpected) {
        %unexpected
          ?? Failure.new(X::Adverb.new(
               :what('{} slice'),
               :source(try { self.VAR.name } // self.WHAT.perl),
               :unexpected(%unexpected.keys)))
          !! self
    }

    proto method ASSIGN-POS(|) is nodal { * }
    multi method ASSIGN-POS(Any:U \SELF: \pos, Mu \assignee) {
       SELF.AT-POS(pos) = assignee;                     # defer < 0 check
    }

    multi method ASSIGN-POS(Any:D: int \pos, Mu \assignee) {
        self.AT-POS(pos) = assignee;                    # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Int:D \pos, Mu \assignee) {
        self.AT-POS(pos) = assignee;                    # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Num:D \pos, Mu \assignee) {
        nqp::isnanorinf(pos)
          ?? Failure.new(X::Item.new(aggregate => self, index => pos))
          !! self.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Any:D \pos, Mu \assignee) {
        self.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Any:U \pos, Mu \assignee) {
        die "Cannot use '{pos.^name}' as an index";
    }
    multi method ASSIGN-POS(Any:D: \one, \two, Mu \assignee) is raw {
        self.AT-POS(one).ASSIGN-POS(two, assignee)
    }
    multi method ASSIGN-POS(Any:D: \one, \two, \three, Mu \assignee) is raw {
        self.AT-POS(one).AT-POS(two).ASSIGN-POS(three, assignee)
    }
    multi method ASSIGN-POS(Any:D: **@indices) {
        my \value := @indices.pop;
        my $final := @indices.pop;
        Rakudo::Internals.WALK-AT-POS(self,@indices).ASSIGN-POS($final,value)
    }

    proto method BIND-POS(|) { * }
    multi method BIND-POS(Any:D: **@indices is raw) is raw {
# looks like Array.pop doesn't really return a bindable container
#        my \value := @indices.pop;
#        my $final := @indices.pop;
#        Rakudo::Internals.WALK-AT-POS(self,@indices).BIND-POS($final,value)

        my int $elems = @indices.elems;   # reifies
        my \value  := @indices.AT-POS(--$elems);
        my $final  := @indices.AT-POS(--$elems);
        my $target := self;
        my int $i = -1;
        $target := $target.AT-POS(@indices.AT-POS($i))
          while nqp::islt_i(++$i,$elems);
        X::Bind.new.throw if $target =:= self;
        $target.BIND-POS($final, value)
    }

    method all()  is nodal { Junction.new("all", self) }
    method any()  is nodal { Junction.new("any", self) }
    method one()  is nodal { Junction.new("one", self) }
    method none() is nodal { Junction.new("none",self) }

    # internals
    proto method AT-KEY(|) is nodal { * }
    multi method AT-KEY(Any:D: $key) is raw {
        Failure.new( self ~~ Associative
          ?? "Associative indexing implementation missing from type {self.WHAT.perl}"
          !! "Type {self.WHAT.perl} does not support associative indexing."
        )
    }
    multi method AT-KEY(Any:U \SELF: \key) is raw {
        nqp::p6bindattrinvres(
          my $scalar,
          Scalar,
          '$!whence',
          -> { nqp::if(
                 nqp::isconcrete(SELF),
                 SELF,
                 (SELF = Hash.new)
               ).BIND-KEY(key, $scalar)
             }
        )
    }

    proto method BIND-KEY(|) is nodal { * }
    multi method BIND-KEY(Any:D: \k, \v) is raw {
        Failure.new(X::Bind.new(target => self.^name))
    }
    multi method BIND-KEY(Any:U \SELF: $key, $BIND ) is raw {
        SELF = Hash.new;
        SELF.BIND-KEY($key, $BIND);
        $BIND
    }

    proto method ASSIGN-KEY(|) is nodal { * }
    multi method ASSIGN-KEY(\SELF: \key, Mu \assignee) is raw {
        SELF.AT-KEY(key) = assignee;
    }

    # XXX GLR review these
    method FLATTENABLE_LIST() is nodal {
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() is nodal { nqp::hash() }

    # XXX GLR do these really need to force a list?
    method Set()     is nodal {     Set.new-from-pairs(self.list) }
    method SetHash() is nodal { SetHash.new-from-pairs(self.list) }
    method Bag()     is nodal {     Bag.new-from-pairs(self.list) }
    method BagHash() is nodal { BagHash.new-from-pairs(self.list) }
    method Mix()     is nodal {     Mix.new-from-pairs(self.list) }
    method MixHash() is nodal { MixHash.new-from-pairs(self.list) }
    method Supply() is nodal { self.list.Supply }

    method nl-out() { "\n" }
    method print-nl() { self.print(self.nl-out) }

    method lazy-if($flag) { self }  # no-op on non-Iterables

    method sum() is nodal {
        my \iter = self.iterator;
        my $sum = 0;
        my Mu $value;
        nqp::until(
          nqp::eqaddr(($value := iter.pull-one),IterationEnd),
          ($sum = $sum + $value)
        );
        $sum;
    }
}
Metamodel::ClassHOW.exclude_parent(Any);

# builtin ops
proto sub infix:<===>(Mu $?, Mu $?) is pure { * }
multi sub infix:<===>($?)    { Bool::True }
multi sub infix:<===>(\a, \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b)
      || (nqp::eqaddr(a.WHAT,b.WHAT)
           && nqp::iseq_s(nqp::unbox_s(a.WHICH), nqp::unbox_s(b.WHICH)))
    )
}

proto sub infix:<before>(Mu $?, Mu $?)  is pure { * }
multi sub infix:<before>($?)      { Bool::True }
multi sub infix:<before>(\a, \b)   { (a cmp b) < 0 }

proto sub infix:<after>(Mu $?, Mu $?) is pure { * }
multi sub infix:<after>($x?)       { Bool::True }
multi sub infix:<after>(\a, \b)    { (a cmp b) > 0 }

proto prefix:<++>(Mu)             { * }
multi prefix:<++>(Mu:D $a is rw) { $a = $a.succ }
multi prefix:<++>(Mu:U $a is rw) { $a = 1 }
proto prefix:<-->(Mu)             { * }
multi prefix:<-->(Mu:D $a is rw) { $a = $a.pred }
multi prefix:<-->(Mu:U $a is rw) { $a = -1 }

proto postfix:<++>(Mu)             { * }
multi postfix:<++>(Mu:D $a is rw) { my $b = $a; $a = $a.succ; $b }
multi postfix:<++>(Mu:U $a is rw) { $a = 1; 0 }
proto postfix:<-->(Mu)             { * }
multi postfix:<-->(Mu:D $a is rw) { my $b = $a; $a = $a.pred; $b }
multi postfix:<-->(Mu:U $a is rw) { $a = -1; 0 }

proto sub pick(|) { * }
multi sub pick($n, +values) { values.pick($n) }

proto sub roll(|) { * }
multi sub roll($n, +values) { values.roll($n) }

proto sub keys(|) { * }
multi sub keys($x) { $x.keys }

proto sub values(|) { * }
multi sub values($x) { $x.values }

proto sub pairs(|) { * }
multi sub pairs($x) { $x.pairs }

proto sub kv(|) { * }
multi sub kv($x) { $x.kv }

proto sub elems(|) is nodal { * }
multi sub elems($a) { $a.elems }

proto sub end(|) { * }
multi sub end($a) { $a.end }

proto sub sum(|) {*}
multi sub sum() { 0 }
multi sub sum(\SELF) { SELF.sum }
multi sub sum(+SELF) { SELF.sum }

sub classify( $test, +items, *%named ) {
    if %named.EXISTS-KEY("into") {
        my $into := %named.DELETE-KEY("into");
        ( $into // $into.new).classify-list($test, items, |%named);
    }
    else {
        Hash.^parameterize(Any,Any).new.classify-list($test, items, |%named);
    }
}
sub categorize( $test, +items, *%named ) {
    if %named.EXISTS-KEY("into") {
        my $into := %named.DELETE-KEY("into");
        ( $into // $into.new).categorize-list($test, items, |%named);
    }
    else {
        Hash.^parameterize(Any,Any).new.categorize-list($test, items, |%named);
    }
}

proto sub item(|) is pure { * }
multi sub item(\x)    { my $ = x }
multi sub item(|c)    { my $ = c.list }
multi sub item(Mu $a) { $a }

sub SLICE_HUH(\SELF, @nogo, %d, %adv) {
    @nogo.unshift('delete')  # recover any :delete if necessary
      if @nogo && @nogo[0] ne 'delete' && %adv.EXISTS-KEY('delete');
    for <delete exists kv p k v> -> $valid { # check all valid params
        if nqp::existskey(%d,nqp::unbox_s($valid)) {
            nqp::deletekey(%d,nqp::unbox_s($valid));
            @nogo.push($valid);
        }
    }

    Failure.new(X::Adverb.new(
      :what<slice>,
      :source(try { SELF.VAR.name } // SELF.WHAT.perl),
      :unexpected(%d.keys),
      :nogo(@nogo),
    ))
} #SLICE_HUH

sub DELETEKEY(Mu \d, str $key) {
    nqp::if(
      nqp::existskey(d,$key),
      nqp::stmts(
        (my Mu $value := nqp::atkey(d,$key)),
        (nqp::deletekey(d,$key)),
        $value
      ),
      Nil
    )
} #DELETEKEY

sub dd(|) {
    my Mu $args := nqp::p6argvmarray();
    if nqp::elems($args) {
        while $args {
            my $var  := nqp::shift($args);
            my $name := try $var.VAR.?name;
            my $type := $var.WHAT.^name;
            my $what := $var.?is-lazy
              ?? $var[^10].perl.chop ~ "... lazy list)"
              !! $var.perl;
            note $name ?? "$type $name = $what" !! $what;
        }
    }
    else { # tell where we are
        note .name ?? "{lc .^name} {.name}" !! "({lc .^name})"
          with callframe(1).code;
    }
    return
}

# vim: ft=perl6 expandtab sw=4
