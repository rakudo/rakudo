my class Hash {
    # Has attributes and parent EnumMap declared in BOOTSTRAP

    method new(*@args) { @args.hash }
    
    method at_key($key is copy) is rw {
        $key = $key.Str;
        self.exists($key)
          ?? pir::find_method__PPs(EnumMap, 'at_key')(self, $key)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $v) } )
    }

    method bind_key($key, \$bindval) is rw {
        pir::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
            nqp::bindattr(self, EnumMap, '$!storage', pir::new__Ps('Hash'));
        nqp::bindkey(
            nqp::getattr(self, EnumMap, '$!storage'),
            nqp::unbox_s($key.Str),
            $bindval)
    }

    multi method perl(Hash:D \$self:) {
        nqp::iscont($self)
          ?? '{' ~ self.pairs.map({.perl}).join(', ') ~ '}'
          !! '(' ~ self.pairs.map({.perl}).join(', ') ~ ').hash'
    }

    method STORE_AT_KEY(Str \$key, Mu $x is copy) is rw {
        pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $x);
    }

    method STORE(\$to_store) {
        my $items = ($to_store,).flat.eager;
        nqp::bindattr(self, EnumMap, '$!storage', pir::new__Ps('Hash'));
        while $items {
            my Mu $x := $items.shift;
            if Enum.ACCEPTS($x) { self.STORE_AT_KEY($x.key.Str, $x.value) }
            elsif EnumMap.ACCEPTS($x) {
                for $x.list { self.STORE_AT_KEY(.key.Str, .value) }
            }
            elsif $items { self.STORE_AT_KEY($x.Str, $items.shift) }
            else {
                die 'Odd number of elements found where hash expected'
            }
        }
        self
    }

    proto method delete(|$) { * }
    multi method delete($key as Str) {
        my Mu $val = self.at_key($key);
        pir::delete(
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
                self!_push_construct($previous.Str, $e);
                $has_previous = 0;
            } elsif $e.^isa(Enum) {
                self!_push_construct($e.key.Str, $e.value);
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

    # push a value onto a hash slot, constructing an array if necessary
    method !_push_construct(Str $key, Mu $value) {
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
        method at_key($key is copy, TValue $v? is copy) is rw {
            $key = $key.Str;
            self.exists($key)
              ?? pir::find_method__PPs(EnumMap, 'at_key')(self, $key)
              !! pir::setattribute__0PPsP($v, Scalar, '$!whence',
                     -> { pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $v) } )
        }
        method STORE_AT_KEY(Str \$key, TValue $x is copy) is rw {
            pir::find_method__PPs(EnumMap, 'STORE_AT_KEY')(self, $key, $x);
        }
        method bind_key($key, TValue \$bindval) is rw {
            pir::defined(nqp::getattr(self, EnumMap, '$!storage')) ||
                nqp::bindattr(self, EnumMap, '$!storage', pir::new__Ps('Hash'));
            nqp::bindkey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key.Str),
                $bindval)
        }
    }
    method PARAMETERIZE_TYPE(Mu $t) {
        self but TypedHash[$t.WHAT]
    }
}


sub circumfix:<{ }>(*@elems) { my $x = Hash.new.STORE(@elems); }
sub hash(*@a, *%h) { my % = @a, %h }
