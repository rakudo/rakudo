my class X::Hash::Store::OddNumber { ... }

my class Hash { # declared in BOOTSTRAP
    # my class Hash is EnumMap {
    #     has $!descriptor;

    method new(*@args) { 
        my %h := nqp::create(self);
        %h.STORE(@args) if @args;
        %h;
    }
    method keyof () { Any }
    
    multi method at_key(Hash:D: $key is copy) is rw {
        my Mu $storage := nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ??
            nqp::getattr(self, EnumMap, '$!storage') !!
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        $key = $key.Str;
        nqp::existskey($storage, nqp::unbox_s($key))
          ?? nqp::atkey($storage, nqp::unbox_s($key))
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::bindkey($storage, nqp::unbox_s($key), $v) } )
    }

    method bind_key($key, Mu \bindval) is rw {
        nqp::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
            nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        nqp::bindkey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s($key.Str),
            bindval)
    }

    multi method perl(Hash:D \SELF:) {
        nqp::iscont(SELF)
          ?? '{' ~ self.pairs.map({.perl}).join(', ') ~ '}'
          !! '(' ~ self.pairs.map({.perl}).join(', ') ~ ').hash'
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

    method STORE_AT_KEY(\key, Mu $x is copy) is rw {
        nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, key, $x);
    }

    method STORE(\to_store) is hidden_from_backtrace {
        my $items = (to_store,).flat.eager;
        nqp::bindattr(self, EnumMap, '$!storage', nqp::hash());
        while $items {
            my Mu $x := $items.shift;
            if Enum.ACCEPTS($x) { self.STORE_AT_KEY($x.key, $x.value) }
            elsif EnumMap.ACCEPTS($x) {
                for $x.list { self.STORE_AT_KEY(.key, .value) }
            }
            elsif $items { self.STORE_AT_KEY($x, $items.shift) }
            else {
                X::Hash::Store::OddNumber.new.throw
            }
        }
        self
    }

    method default() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.default;
    }

    proto method delete(|) { * }
    multi method delete(Hash:U:) { Nil }
    multi method delete($key as Str) {
        my Mu $val = self.at_key($key);
        nqp::deletekey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s($key)
        );
        $val;
    }
    multi method delete(@keys) {
        @keys.map({ self.delete($^key) })
    }
    multi method delete(*@keys) {
        @keys.map({ self.delete($^key) })
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
        if self.exists($key) {
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
        method at_key(::?CLASS:D: $key is copy, TValue $v? is copy) is rw {
            $key = $key.Str;
            self.exists($key)
              ?? nqp::findmethod(EnumMap, 'at_key')(self, $key)
              !! nqp::p6bindattrinvres($v, Scalar, '$!whence',
                     -> { nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, $key, $v) } )
        }
        method STORE_AT_KEY(Str \key, TValue $x is copy) is rw {
            nqp::findmethod(EnumMap, 'STORE_AT_KEY')(self, key, $x);
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
              ~ self.pairs.map({.perl}).join(', ')
              ~ ')';
        }
    }
    my role TypedHash[::TValue, ::TKey] does Associative[TValue] {
        has $!keys;
        method keyof () { TKey }
        method at_key(::?CLASS:D: TKey \key, TValue $v? is copy) is rw {
            my $key_which = key.WHICH;
            self.exists(key)
              ?? nqp::findmethod(EnumMap, 'at_key')(self, $key_which)
              !! nqp::p6bindattrinvres($v, Scalar, '$!whence',
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
                            $v);
                    })
        }
        method STORE_AT_KEY(TKey \key, TValue $x is copy) is rw {
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
                $x);
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
        method exists(TKey \key) {
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
              ~ self.pairs.map({.perl}).join(', ')
              ~ ')';
        }
    }
    method PARAMETERIZE_TYPE(Mu $t, |c) {
        if c.elems == 0 {
            self but TypedHash[$t.WHAT]
        }
        elsif c.elems == 1 {
            self but TypedHash[$t.WHAT, c[0]]
        }
        else {
            die "Can only type-constraint Hash with [ValueType] or [ValueType,KeyType]";
        }
    }
}


sub circumfix:<{ }>(*@elems) { my $ = Hash.new(@elems) }
sub hash(*@a, *%h) { my % = @a, %h }
