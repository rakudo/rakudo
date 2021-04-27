my class Pair                   { ... }
my class Range                  { ... }
my class Seq                    { ... }
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

    multi method ACCEPTS(Any:D: Mu:U --> False) { }
    multi method ACCEPTS(Any:D: Mu:D \topic) {
        # XXX: &[===] works with Any, not Mu!
        self === topic
    }

    proto method EXISTS-KEY(|) is nodal {*}
    multi method EXISTS-KEY(Any:U: $ --> False) { }
    multi method EXISTS-KEY(Any:D: $ --> False) { }

    proto method DELETE-KEY(|) is nodal {*}
    multi method DELETE-KEY(Any:U: $ --> Nil) { }
    multi method DELETE-KEY(Any:D: $) {
        Failure.new("Can not remove values from a {self.^name}")
    }

    proto method DELETE-POS(|) is nodal {*}
    multi method DELETE-POS(Any:U: $pos --> Nil) { }
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

    proto method list(|) is nodal {*}
    multi method list(Any:U:) { infix:<,>(self) }
    multi method list(Any:D \SELF:) { infix:<,>(SELF) }

    proto method flat(|) is nodal {*}
    multi method flat() { self.list.flat }

    proto method eager(|) is nodal {*}
    multi method eager() { self.list.eager }

    proto method serial(|) is nodal {*}
    multi method serial() { self }

    # derived from .list
    proto method List(|) is nodal {*}
    multi method List() { self.list }
    proto method Slip(|) is nodal {*}
    multi method Slip() { self.list.Slip }
    proto method Array(|) is nodal {*}
    multi method Array() { self.list.Array }
    proto method Seq(|) is nodal {*}
    multi method Seq() { Seq.new(self.iterator) }

    proto method hash(|) is nodal {*}
    multi method hash(Any:U:) { my % = () }
    multi method hash(Any:D:) { my % = self }

    # derived from .hash
    proto method Hash(|) is nodal {*}
    multi method Hash() { self.hash.Hash }

    proto method Map(|) is nodal {*}
    multi method Map() { Map.new(self) }

    proto method elems(|) is nodal {*}
    multi method elems(Any:U: --> 1) { }
    multi method elems(Any:D:) { self.list.elems }

    proto method end(|) is nodal {*}
    multi method end(Any:U: --> 0) { }
    multi method end(Any:D:) { self.list.end }

    proto method keys(|) is nodal {*}
    multi method keys(Enumeration:) is default { self.enums.keys }
    multi method keys(Bool:)        { self.enums.keys }
    multi method keys(Any:U:) { () }
    multi method keys(Any:D:) { self.list.keys }

    proto method kv(|) is nodal {*}
    multi method kv(Enumeration:) is default { self.enums.kv }
    multi method kv(Bool:)        { self.enums.kv }
    multi method kv(Any:U:) { () }
    multi method kv(Any:D:) { self.list.kv }

    proto method values(|) is nodal {*}
    multi method values(Enumeration:) is default { self.enums.values }
    multi method values(Bool:)        { self.enums.values }
    multi method values(Any:U:) { () }
    multi method values(Any:D:) { self.list }

    proto method pairs(|) is nodal {*}
    multi method pairs(Enumeration:) is default { self.enums.pairs }
    multi method pairs(Bool:)        { self.enums.pairs }
    multi method pairs(Any:U:) { () }
    multi method pairs(Any:D:) { self.list.pairs }

    proto method antipairs(|) is nodal {*}
    multi method antipairs(Enumeration:) is default { self.enums.antipairs }
    multi method antipairs(Bool:)        { self.enums.antipairs }
    multi method antipairs(Any:U:) { () }
    multi method antipairs(Any:D:) { self.list.antipairs }

    proto method invert(|) is nodal {*}
    multi method invert(Enumeration:) is default { self.enums.invert }
    multi method invert(Bool:)        { self.enums.invert }
    multi method invert(Any:U:) { () }
    multi method invert(Any:D:) { self.list.invert }

    proto method splice(|) is nodal {*}

    proto method pick(|) is nodal {*}
    multi method pick()   { self.list.pick     }
    multi method pick($n) { self.list.pick($n) }

    proto method roll(|) is nodal {*}
    multi method roll()   { self.list.roll     }
    multi method roll($n) { self.list.roll($n) }

    multi method iterator(Any:) { self.list.iterator }

    method match(Any:U: |) { self.Str; nqp::getlexcaller('$/') = Nil }

    proto method classify(|) is nodal {*}
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

    proto method categorize(|) is nodal {*}
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

    # XXX GLR Do we need tree post-GLR?
    proto method tree(|) is nodal {*}
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

    proto method append(|) is nodal {*}
    multi method append(Any:U \SELF: |values) {
        SELF = nqp::istype(SELF,Positional) ?? SELF.new !! Array.new;
        SELF.append(|values);
    }

    proto method unshift(|) is nodal {*}
    multi method unshift(Any:U \SELF: |values) {
        SELF = Array.new;
        SELF.unshift(|values);
    }

    proto method prepend(|) is nodal {*}
    multi method prepend(Any:U \SELF: |values) {
        SELF = Array.new;
        SELF.prepend(|values);
    }

    proto method EXISTS-POS(|) is nodal {*}
    multi method EXISTS-POS(Any:U: Any:D $ --> False) { }
    multi method EXISTS-POS(Any:U: Any:U $pos) {
        die "Cannot use '{$pos.^name}' as an index";
    }

    multi method EXISTS-POS(Any:D: Int:D \pos --> Bool:D) {
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Num:D \pos --> Bool:D) {
        X::Item.new(aggregate => self, index => pos).throw
          if nqp::isnanorinf(pos);
        self.AT-POS(nqp::unbox_i(pos.Int));
        pos == 0;
    }
    multi method EXISTS-POS(Any:D: Any:D \pos --> Bool:D) {
        pos.Int == 0;
    }
    multi method EXISTS-POS(Any:D: Any:U \pos) {
        die "Cannot use '{pos.^name}' as an index";
    }
    multi method EXISTS-POS(Any:D: \one, \two --> Bool:D) is raw {
        nqp::istype((my $one := self.AT-POS(one)),Failure)
          ?? False
          !! $one.EXISTS-POS(two)
    }
    multi method EXISTS-POS(Any:D: \one, \two,\three --> Bool:D) is raw {
        nqp::istype((my $one := self.AT-POS(one)),Failure)
          || nqp::istype((my $two := $one.AT-POS(two)),Failure)
          ?? False
          !! $two.EXISTS-POS(three)
    }
    multi method EXISTS-POS(Any:D: **@indices --> Bool:D) {
        my $final    := @indices.pop;  # also reifies
        my $target   := self;
        my $indices  := nqp::getattr(@indices,List,'$!reified');
        my int $elems = nqp::elems($indices);
        my int $i     = -1;
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::if(
            nqp::istype(
              ($target := $target.AT-POS(nqp::atpos($indices,$i))),
              Failure
            ),
            (return False)
          )
        );
        $target.EXISTS-POS($final)
    }

    proto method AT-POS(|) is nodal {*}
    multi method AT-POS(Any:U \SELF: Int:D \pos) is raw {
        nqp::p6scalarfromcertaindesc(ContainerDescriptor::VivifyArray.new(SELF, pos))
    }
    multi method AT-POS(Any:U: Num:D \pos) is raw {
        nqp::isnanorinf(pos)
          ?? Failure.new(X::Item.new(aggregate => self, index => pos))
          !! self.AT-POS(nqp::unbox_i(pos.Int))
    }
    multi method AT-POS(Any:U: Any:D \pos) is raw {
        self.AT-POS(nqp::unbox_i(pos.Int));
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

    proto method ZEN-KEY(|) {*}
    multi method ZEN-KEY(*%unexpected) {
        %unexpected
          ?? Failure.new(X::Adverb.new(
               :what('{} slice'),
               :source(try { self.VAR.name } // self.WHAT.raku),
               :unexpected(%unexpected.keys)))
          !! self
    }

    proto method ASSIGN-POS(|) is nodal {*}
    multi method ASSIGN-POS(Any:U \SELF: \pos, Mu \assignee) is raw {
       SELF.AT-POS(pos) = assignee;                     # defer < 0 check
    }

    multi method ASSIGN-POS(Any:D \SELF: Int:D \pos, Mu \assignee) is raw {
        SELF.AT-POS(pos) = assignee;                    # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D \SELF: Num:D \pos, Mu \assignee) is raw {
        nqp::isnanorinf(pos)
          ?? Failure.new(X::Item.new(aggregate => SELF, index => pos))
          !! SELF.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D \SELF: Any:D \pos, Mu \assignee) is raw {
        SELF.AT-POS(nqp::unbox_i(pos.Int)) = assignee;  # defer < 0 check
    }
    multi method ASSIGN-POS(Any:D: Any:U \pos, Mu \assignee) {
        die "Cannot use '{pos.^name}' as an index";
    }
    multi method ASSIGN-POS(Any:D \SELF: \one, \two, Mu \assignee) is raw {
        SELF.AT-POS(one).ASSIGN-POS(two, assignee)
    }
    multi method ASSIGN-POS(Any:D \SELF: \one, \two, \three, Mu \assignee) is raw {
        SELF.AT-POS(one).AT-POS(two).ASSIGN-POS(three, assignee)
    }
    multi method ASSIGN-POS(Any:D \SELF: **@indices) is raw {
        my \value := @indices.pop;
        my $final := @indices.pop;
        Rakudo::Internals.WALK-AT-POS(SELF,@indices).ASSIGN-POS($final,value)
    }

    proto method BIND-POS(|) {*}
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
        X::Bind.new.throw if nqp::eqaddr($target,self);
        $target.BIND-POS($final, value)
    }

    method all()  is nodal { Junction.new("all", self) }
    method any()  is nodal { Junction.new("any", self) }
    method one()  is nodal { Junction.new("one", self) }
    method none() is nodal { Junction.new("none",self) }

    # internals
    proto method AT-KEY(|) is nodal {*}
    multi method AT-KEY(Any:D: $key) is raw {
        Failure.new( self ~~ Associative
          ?? "Associative indexing implementation missing from type {self.WHAT.raku}"
          !! "Type {self.WHAT.raku} does not support associative indexing."
        )
    }
    multi method AT-KEY(Any:U \SELF: \key) is raw {
        nqp::p6scalarfromcertaindesc(ContainerDescriptor::VivifyHash.new(SELF, key))
    }

    proto method BIND-KEY(|) is nodal {*}
    multi method BIND-KEY(Any:D: \k, \v) is raw {
        X::Bind.new(target => self.^name).throw
    }
    multi method BIND-KEY(Any:U \SELF: $key, $BIND ) is raw {
        SELF = Hash.new;
        SELF.BIND-KEY($key, $BIND);
        $BIND
    }

    proto method ASSIGN-KEY(|) is nodal {*}
    multi method ASSIGN-KEY(\SELF: \key, Mu \assignee) is raw {
        SELF.AT-KEY(key) = assignee;
    }

    # XXX GLR review these
    method FLATTENABLE_LIST() is nodal {
        my $list := self.list;
        nqp::findmethod($list, 'FLATTENABLE_LIST')($list);
    }
    method FLATTENABLE_HASH() is nodal { nqp::hash() }

    proto method Set(|) is nodal {*}
    multi method Set(Any: --> Set:D) {
        Set.new-from-pairs(self.list)
    }

    proto method SetHash(|) is nodal {*}
    multi method SetHash(Any: --> SetHash:D) {
        SetHash.new-from-pairs(self.list)
    }

    proto method Bag(|) is nodal {*}
    multi method Bag(Any: --> Bag:D) {
        Bag.new-from-pairs(self.list)
    }

    proto method BagHash(|) is nodal {*}
    multi method BagHash(Any: --> BagHash:D) {
        BagHash.new-from-pairs(self.list)
    }

    proto method Mix(|) is nodal {*}
    multi method Mix(Any: --> Mix:D) {
        Mix.new-from-pairs(self.list)
    }

    proto method MixHash(|) is nodal {*}
    multi method MixHash(Any: --> MixHash:D) {
        MixHash.new-from-pairs(self.list)
    }

    # XXX GLR does this really need to force a list?
    proto method Supply(|) is nodal {*}
    multi method Supply() { self.list.Supply }

    method nl-out(--> Str:D) { "\n" }
    method print-nl() { self.print(self.nl-out) }

    method lazy-if($flag) { self }  # no-op on non-Iterables
}
Metamodel::ClassHOW.exclude_parent(Any);

# builtin ops
proto sub infix:<===>($?, $?, *%) is pure {*}
multi sub infix:<===>($? --> True) { }
multi sub infix:<===>(\a, \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
      || (nqp::eqaddr(a.WHAT,b.WHAT)
           && nqp::iseq_s(nqp::unbox_s(a.WHICH), nqp::unbox_s(b.WHICH)))
    )
}

proto sub prefix:<++>(Mu, *%)        {*}
multi sub prefix:<++>(Mu:D $a is rw) { $a = $a.succ }
multi sub prefix:<++>(Mu:U $a is rw) { $a = 1 }
proto sub prefix:<-->(Mu, *%)        {*}
multi sub prefix:<-->(Mu:D $a is rw) { $a = $a.pred }
multi sub prefix:<-->(Mu:U $a is rw) { $a = -1 }

proto sub postfix:<++>(Mu, *%)        {*}
multi sub postfix:<++>(Mu:D $a is rw) { my $b = $a; $a = $a.succ; $b }
multi sub postfix:<++>(Mu:U $a is rw) { $a = 1; 0 }
proto sub postfix:<-->(Mu, *%)        {*}
multi sub postfix:<-->(Mu:D $a is rw) { my $b = $a; $a = $a.pred; $b }
multi sub postfix:<-->(Mu:U $a is rw) { $a = -1; 0 }

proto sub pick($, |) {*}
multi sub pick($n, +values) { values.pick($n) }

proto sub roll($, |) {*}
multi sub roll($n, +values) { values.roll($n) }

proto sub keys($, *%) {*}
multi sub keys($x) { $x.keys }

proto sub values($, *%) {*}
multi sub values($x) { $x.values }

proto sub pairs($, *%) {*}
multi sub pairs($x) { $x.pairs }

proto sub kv($, *%) {*}
multi sub kv($x) { $x.kv }

proto sub elems($, *%) is nodal {*}
multi sub elems($a) { $a.elems }

proto sub end($, *%) {*}
multi sub end($a) { $a.end }

proto sub sum(|) {*}
multi sub sum() { 0 }
multi sub sum(\SELF) { SELF.sum }
multi sub sum(+SELF) { SELF.sum }

proto sub classify($, |) {*}
multi sub classify($test, +items, :$into!, *%named ) {
    ( $into // $into.new).classify-list($test, items, |%named)
}
multi sub classify($test, +items, *%named ) {
    Hash.^parameterize(Any,Any).new.classify-list($test, items, |%named);
}

proto sub categorize($, |) {*}
multi sub categorize($test, +items, :$into!, *%named ) {
    ( $into // $into.new).categorize-list($test, items, |%named)
}
multi sub categorize($test, +items, *%named ) {
    Hash.^parameterize(Any,Any).new.categorize-list($test, items, |%named)
}

proto sub item(|) is pure {*}
multi sub item(\x)    { my $ = x }
multi sub item(|c)    { my $ = c.list }
multi sub item(Mu $a) { $a }

sub dd(|c) {  # is implementation-detail

    # handler for BOOTxxxArrays
    sub BOOTArray(Mu \array) {
        my \buffer  := nqp::create(IterationBuffer);
        my \clone   := nqp::clone(array);
        my str $name = array.^name;
        if $name eq 'BOOTIntArray' {
            nqp::while(
              clone,
              nqp::push(buffer,nqp::shift_i(clone))
            );
        }
        elsif $name eq 'BOOTStrArray' {
            nqp::while(
              clone,
              nqp::push(buffer,nqp::shift_s(clone))
            );
        }
        elsif $name eq 'BOOTNumArray' {
            nqp::while(
              clone,
              nqp::push(buffer,nqp::shift_n(clone))
            );
        }
        else {
            nqp::while(
              clone,
              nqp::push(buffer,nqp::shift(clone))
            );
        }
        $name ~ buffer.List.raku
    }

    # handler for BOOTContext
    sub BOOTContext(Mu \context) {
        my $hash := nqp::hash;
        my \iterator := nqp::iterator(context);
        nqp::while(
          iterator,
          nqp::bindkey(
            $hash,
            nqp::iterkey_s(nqp::shift(iterator)),
            nqp::iterval(iterator)
          )
        );
        context.^name ~ '(' ~ nqp::substr(nqp::hllize($hash).raku.chop,1) ~ ')'
    }

    # handler for BOOTThread
    sub BOOTThread(Mu \thread) {
        "VM thread object for thread #{
            nqp::threadid(thread)
        } with { nqp::threadlockcount(thread) } locks"
    }

    my Mu $args := nqp::p6argvmarray();
    if nqp::elems($args) {
        while $args {
            my $var  := nqp::shift($args);
            my $name := ! nqp::istype($var.VAR, Failure) && try $var.VAR.name;
            my $type := $var.WHAT.^name.split("::").tail;
            my $what := nqp::can($var,'raku')
              ?? $var.raku
              !! nqp::can($var,'perl')
                ?? $var.perl
                !! $var.^name.starts-with('BOOT')
                  ?? $var.^name.ends-with('Array')
                    ?? BOOTArray($var)
                    !! $var.^name.ends-with('Context')
                      ?? BOOTContext($var)
                      !! $var.^name.ends-with('Thread')
                        ?? BOOTThread($var)
                        !! "($var.^name() without .raku or .perl method)"
                !! "($var.^name() without .raku or .perl method)";
            note $name ?? "$type $name = $what" !! $what;
        }
    }
    elsif c.hash -> %named {
        note .raku for %named.sort: { .key }
    }
    else { # tell where we are
        note .name
          ?? "{lc .^name} {.name}{.signature.gist}"
          !! "{lc .^name} {.signature.gist}"
          with callframe(1).code;
    }
    return
}

# vim: expandtab shiftwidth=4
