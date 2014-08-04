my class X::Hash::Store::OddNumber { ... }

my class Hash { # declared in BOOTSTRAP
    # my class Hash is EnumMap {
    #     has Mu $!descriptor;

    method new(*@args) { 
        my %h := nqp::create(self);
        %h.STORE(@args) if @args;
        %h;
    }

    multi method at_key(Hash:D: \key) is rw {
        my Mu $storage := nqp::getattr(self, EnumMap, '$!storage');
        $storage := nqp::bindattr(self, EnumMap, '$!storage', nqp::hash())
            unless nqp::defined($storage);
        my str $key = nqp::istype(key, Str) ?? key !! key.Str;
        if nqp::existskey($storage, $key) {
            nqp::atkey($storage, $key);
        }
        else {
            nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                Scalar,
                '$!whence',
                -> { nqp::bindkey($storage, $key, v) }
            );
        }
    }

    multi method assign_key(Hash:D: \key, Mu \assignval) {
        my Mu $storage := nqp::getattr(self, EnumMap, '$!storage');
        $storage := nqp::bindattr(self, EnumMap, '$!storage', nqp::hash())
            unless nqp::defined($storage);
        my str $key = nqp::istype(key, Str) ?? key !! key.Str;
        nqp::existskey($storage, $key)
            ?? (nqp::atkey($storage, $key) = assignval)
            !! nqp::bindkey($storage, $key,
                nqp::p6scalarfromdesc($!descriptor) = assignval)
    }

    method bind_key(Hash:D: \key, Mu \bindval) is rw {
        my Mu $storage := nqp::getattr(self, EnumMap, '$!storage');
        $storage := nqp::bindattr(self, EnumMap, '$!storage', nqp::hash())
            unless nqp::defined($storage);
        my str $key = nqp::istype(key, Str) ?? key !! key.Str;
        nqp::bindkey($storage, $key, bindval)
    }

    multi method perl(Hash:D \SELF:) {
        nqp::iscont(SELF)
          ?? '{' ~ self.pairs.pick(*).map({.perl}).join(', ') ~ '}'
          !! '(' ~ self.pairs.pick(*).map({.perl}).join(', ') ~ ').hash'
    }

    multi method gist(Hash:D \SELF:) {
        nqp::iscont(SELF)
          ?? '{' ~ self.pairs.sort.map({.perl}).join(', ') ~ '}'
          !! '(' ~ self.pairs.sort.map({.perl}).join(', ') ~ ').hash'
    }

    multi method DUMP(Hash:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!descriptor');
        nqp::push($attrs,  $!descriptor );
        nqp::push($attrs, '$!storage'   );
        nqp::push($attrs,  nqp::getattr(nqp::decont(self), EnumMap, '$!storage'));
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }

    method STORE_AT_KEY(\key, Mu $x) is rw {
        my $v := nqp::p6scalarfromdesc($!descriptor);
        nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, key, $v = $x);
    }

    method STORE(\to_store) {
        my $items = (to_store,).flat.eager;
        nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());

        if $items.elems == 1 {
            if nqp::istype($items[0],EnumMap) {
                my Mu $x := $items.shift;
                DEPRECATED(
                  self.VAR.name ~ ' = %(itemized hash)',
                  :what(self.VAR.name ~ ' = itemized hash')
                ) if nqp::iscont($x);
                for $x.list { self.STORE_AT_KEY(.key, .value) }
                return self;
            }
        }

        while $items {
            my Mu $x := $items.shift;
            if nqp::istype($x,Enum) { self.STORE_AT_KEY($x.key, $x.value) }
            elsif nqp::istype($x,EnumMap) and !nqp::iscont($x) {
                for $x.list { self.STORE_AT_KEY(.key, .value) }
            }
            elsif $items { self.STORE_AT_KEY($x, $items.shift) }
            else {
                X::Hash::Store::OddNumber.new.throw
            }
        }
        self
    }

    # introspection
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Str !! $d.name()
    }
    method keyof () { Any }
    method of() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.of;
    }
    method default() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.default;
    }
    method dynamic() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! so $d.dynamic;
    }

    proto method delete(|) { * }
    multi method delete(Hash:U:) {  # is DEPRECATED doesn't work in settings
        DEPRECATED('the :delete adverb with postcircumfix:<{ }>');
        self.delete_key;
    }
    multi method delete($key as Str) {  # is DEPRECATED doesn't work in settings
        DEPRECATED('the :delete adverb with postcircumfix:<{ }>');
        self.delete_key($key);
    }

    proto method delete_key(|) { * }
    multi method delete_key(Hash:U:) { Nil }
    multi method delete_key(Str:D \key) {
        my Mu $val = self.at_key(key);
        nqp::deletekey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s(key)
        );
        $val;
    }
    multi method delete_key(Str:D \key, :$SINK!) {
        nqp::deletekey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s(key)
        );
        Nil;
    }
    multi method delete_key(\key as Str) {
        my Mu $val = self.at_key(key);
        nqp::deletekey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s(key)
        );
        $val;
    }
    multi method delete_key(\key as Str, :$SINK!) {
        nqp::deletekey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s(key)
        );
        Nil;
    }

    method push(*@values) {
        my $previous;
        my $has_previous;
        for @values -> $e {
            if $has_previous {
                self!_push_construct($previous, $e);
                $has_previous = 0;
            } elsif $e.^isa(Enum) {
                self!_push_construct($e.key, $e.value);
            } else {
                $previous = $e;
                $has_previous = 1;
            }
        }
        if $has_previous {
            warn "Trailing item in Hash.push";
        }
        self
    }

    proto method classify-list(|) { * }
    multi method classify-list( &test, *@list ) {
        fail 'Cannot .classify an infinite list' if @list.infinite;
        if @list {

            # multi-level classify
            if test(@list[0]) ~~ List {
                for @list -> $l {
                    my @keys  = test($l);
                    my $last := @keys.pop;
                    my $hash  = self;
                    $hash = $hash{$_} //= self.new for @keys;
                    nqp::push(
                      nqp::p6listitems(nqp::decont($hash{$last} //= [])), $l );
                }
            }

            # just a simple classify
            else {
                nqp::push(
                  nqp::p6listitems(nqp::decont(self{test $_} //= [])), $_ )
                  for @list;
            }
        }
        self;
    }
    multi method classify-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method classify-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }

    proto method categorize-list(|) { * }
    multi method categorize-list( &test, *@list ) {
        fail 'Cannot .categorize an infinite list' if @list.infinite;
        if @list {

            # multi-level categorize
            if test(@list[0])[0] ~~ List {
                for @list -> $l {
                    for test($l) -> $k {
                        my @keys = @($k);
                        my $last := @keys.pop;
                        my $hash  = self;
                        $hash = $hash{$_} //= self.new for @keys;
                        nqp::push(
                          nqp::p6listitems(
                            nqp::decont($hash{$last} //= [])), $l );
                    }
                }
            }

            # just a simple categorize
            else {
                for @list -> $l {
                    nqp::push(
                      nqp::p6listitems(nqp::decont(self{$_} //= [])), $l )
                      for test($l);
                }
            }
        }
        self;
    }
    multi method categorize-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method categorize-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }

    # push a value onto a hash slot, constructing an array if necessary
    method !_push_construct(Mu $key, Mu $value) {
        if self.exists_key($key) {
            if self.{$key}.^isa(Array) {
                self.{$key}.push($value);
            } else {
                my Mu $tmp = self.{$key};
                self.{$key} = [ $tmp, $value];
            }
        } else {
            self.{$key} = $value;
        }
    }
    
    my role TypedHash[::TValue] does Associative[TValue] {
        method at_key(::?CLASS:D: $key is copy) is rw {
            $key = $key.Str;
            if self.exists_key($key) {
                nqp::findmethod(EnumMap, 'at_key')(self, $key);
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> { nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, $key, v) }
                );
            }
        }
        method STORE_AT_KEY(Str \key, TValue $x) is rw {
            my $v :=
              nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor'));
            nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, key, $v = $x);
        }
        multi method assign_key(::?CLASS:D: \key, TValue \assignval) {
            my Mu $storage := nqp::getattr(self, EnumMap, '$!storage');
            $storage := nqp::bindattr(self, EnumMap, '$!storage', nqp::hash())
                unless nqp::defined($storage);
            my str $key = nqp::istype(key, Str) ?? key !! key.Str;
            if nqp::existskey($storage, $key) {
                nqp::atkey($storage, $key) = assignval;
            }
            else {
                nqp::bindkey($storage, $key,
                    nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor')) = assignval)
            }
        }
        method bind_key($key, TValue \bindval) is rw {
            nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
                nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
            nqp::bindkey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key.Str),
                bindval)
        }
        multi method perl(::?CLASS:D \SELF:) {
            'Hash['
              ~ TValue.perl
              ~ '].new('
              ~ self.pairs.pick(*).map({.perl}).join(', ')
              ~ ')';
        }
    }
    my role TypedHash[::TValue, ::TKey] does Associative[TValue] {
        has $!keys;
        method keyof () { TKey }
        method at_key(::?CLASS:D: TKey \key) is rw {
            my $key_which = key.WHICH;
            if self.exists_key(key) {
                nqp::findmethod(EnumMap, 'at_key')(self, $key_which);
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> {
                        nqp::defined(nqp::getattr(self, $?CLASS, '$!keys')) ||
                            nqp::bindattr(self, $?CLASS, '$!keys', nqp::hash());
                        nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
                            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
                        nqp::bindkey(
                            nqp::getattr(self, $?CLASS, '$!keys'),
                            nqp::unbox_s($key_which),
                            key);                        
                        nqp::bindkey(
                            nqp::getattr(self, EnumMap, '$!storage'),
                            nqp::unbox_s($key_which),
                            v);
                    });
            }
        }
        method STORE_AT_KEY(TKey \key, TValue $x) is rw {
            my $key_which = key.WHICH;
            nqp::defined(nqp::getattr(self, $?CLASS, '$!keys')) ||
                nqp::bindattr(self, $?CLASS, '$!keys', nqp::hash());
            nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
                nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
            nqp::bindkey(
                nqp::getattr(self, $?CLASS, '$!keys'),
                nqp::unbox_s($key_which),
                key);
            my $v :=
              nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor'));
            nqp::bindkey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key_which),
                $v = $x);
        }
        method assign_key(::?CLASS:D: TKey \key, TValue \assignval) {
            my Mu $storage := nqp::getattr(self, EnumMap, '$!storage');
            $storage := nqp::bindattr(self, EnumMap, '$!storage', nqp::hash())
                unless nqp::defined($storage);
            my str $key_which = nqp::unbox_s(key.WHICH);
            if nqp::existskey($storage, $key_which) {
                nqp::atkey($storage, $key_which) = assignval;
            }
            else {
                nqp::defined(nqp::getattr(self, $?CLASS, '$!keys')) ||
                    nqp::bindattr(self, $?CLASS, '$!keys', nqp::hash());
                nqp::bindkey(nqp::getattr(self, $?CLASS, '$!keys'), $key_which, key);
                nqp::bindkey($storage, $key_which,
                    nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor')) = assignval)
            }
        }
        method bind_key(TKey \key, TValue \bindval) is rw {
            my $key_which = key.WHICH;
            nqp::defined(nqp::getattr(self, $?CLASS, '$!keys')) ||
                nqp::bindattr(self, $?CLASS, '$!keys', nqp::hash());
            nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
                nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
            nqp::bindkey(
                nqp::getattr(self, $?CLASS, '$!keys'),
                nqp::unbox_s($key_which),
                key);
            nqp::bindkey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key_which),
                bindval)
        }
        method exists (TKey \key) {  # is DEPRECATED doesn't work in settings
            DEPRECATED('the :exists adverb with postcircumfix:<{ }>');
            self.exists_key(key);
        }
        method exists_key(TKey \key) {
            nqp::defined($!keys)
              ?? nqp::p6bool(nqp::existskey($!keys, nqp::unbox_s(key.WHICH)))
              !! False
        }
        method keys(EnumMap:) {
            return unless self.DEFINITE && nqp::defined($!keys);
            HashIter.new(self, :keystore($!keys), :k).list
        }
        method kv(EnumMap:) {
            return unless self.DEFINITE && nqp::defined($!keys);
            HashIter.new(self, :keystore($!keys), :kv).list
        }
        method values(EnumMap:) {
            return unless self.DEFINITE && nqp::defined($!keys);
            HashIter.new(self, :keystore($!keys), :v).list
        }
        method pairs(EnumMap:) {
            return unless self.DEFINITE && nqp::defined($!keys);
            HashIter.new(self, :keystore($!keys), :pairs).list
        }
        method invert(EnumMap:) {
            return unless self.DEFINITE && nqp::defined($!keys);
            HashIter.new(self, :keystore($!keys), :invert).list
        }
        multi method perl(::?CLASS:D \SELF:) {
            'Hash['
              ~ TValue.perl
              ~ ','
              ~ TKey.perl
              ~ '].new('
              ~ self.pairs.pick(*).map({.perl}).join(', ')
              ~ ')';
        }
        multi method delete_key($key) {
            my Mu $val = self.at_key($key);
            my $key-which = $key.WHICH;

            nqp::deletekey(
                nqp::getattr(self, $?CLASS, '$!keys'),
                nqp::unbox_s($key-which)
            );

            nqp::deletekey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key-which)
            );
            $val;
        }
    }
    method PARAMETERIZE_TYPE(Mu $t, |c) {
        if c.elems == 0 {
#            my $what := self but TypedHash[$t.WHAT]; # too early in bootstrap
            my $what := self.HOW.mixin(self.WHAT, TypedHash[$t.WHAT]);
            # needs to be done in COMPOSE phaser when that works
            $what.HOW.set_name(self,"{self.HOW.name(self)}[{$t.HOW.name($t)}]");
            $what;
        }
        elsif c.elems == 1 {
            my $what := self.HOW.mixin(self.WHAT, TypedHash[$t.WHAT,c[0]]);
#            my $what := self but TypedHash[$t.WHAT, c[0]]; # too early in bootstrap
            # needs to be done in COMPOSE phaser when that works
            $what.HOW.set_name(self,"{self.HOW.name(self)}[{$t.HOW.name($t)},{c[0].HOW.name(c[0])}]");
            $what;
        }
        else {
            die "Can only type-constraint Hash with [ValueType] or [ValueType,KeyType]";
        }
    }
}


sub circumfix:<{ }>(*@elems) { my $ = Hash.new(@elems) }
sub hash(*@a, *%h) { my % = @a, %h }

# vim: ft=perl6 expandtab sw=4
