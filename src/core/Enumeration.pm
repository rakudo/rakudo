# Method that we have on enumeration types.
my role Enumeration {
    has $.key;
    has $.value;

    method enums() { self.^enum_values.Map }

    multi method kv(::?CLASS:D:) { ($!key, $!value) }
    method pair(::?CLASS:D:) { $!key => $!value }

    multi method gist(::?CLASS:D:) { $!key                     }
    multi method perl(::?CLASS:D:) { self.^name ~ '::' ~ $!key }

    multi method pick(::?CLASS:U:)       { self.^enum_value_list.pick     }
    multi method pick(::?CLASS:U: \n)    { self.^enum_value_list.pick(n)  }
    multi method pick(::?CLASS:D: *@pos) { self xx +?( @pos[0] // 1 )     }
    multi method roll(::?CLASS:U:)       { self.^enum_value_list.roll     }
    multi method roll(::?CLASS:U: \n)    { self.^enum_value_list.roll(n)  }
    multi method roll(::?CLASS:D: *@pos) { self xx +?( @pos[0] // 1 )     }

    multi method Numeric(::?CLASS:D:) { $!value.Numeric }
    multi method Int(::?CLASS:D:)     { $!value.Int }
    multi method Real(::?CLASS:D:)    { $!value.Real }

    multi method WHICH(::?CLASS:D:) { 
        nqp::box_s(
          nqp::join("|",nqp::list(self.^name,$!key,$!value.WHICH)),
          ObjAt
        )
    }

    # Make sure we always accept any element of the enumeration
    multi method ACCEPTS(::?CLASS:D: ::?CLASS:U $ --> True) { }
    multi method ACCEPTS(::?CLASS:D: ::?CLASS:D \v) { self === v }

    method CALL-ME(|) {
        my $x := nqp::atpos(nqp::p6argvmarray(), 1).AT-POS(0);
        nqp::istype($x, ::?CLASS)
            ?? $x
            !! self.^enum_from_value($x)
    }

    method pred() {
        my @values := self.^enum_value_list;
        my $index   = @values.first( self, :k );
        return $index <= 0 ?? self !! @values[ $index - 1 ];
    }
    method succ() {
        my @values := self.^enum_value_list;
        my $index   = @values.first( self, :k );
        return $index >= @values.end ?? self !! @values[ $index + 1 ];
    }
}

# Methods that we also have if the base type of an enumeration is
# Numeric.
my role NumericEnumeration {
    multi method Str(::?CLASS:D:) {
        self.key
    }
}
my role StringyEnumeration {
    multi method Str(::?CLASS:D:) {
        self.value
    }
}

sub ENUM_VALUES(*@args) {
    my Mu $prev = -1;
    my %res;
    for @args {
        if .^isa(Pair) {
            %res{.key} = $prev = .value;
        }
        else {
            %res{$_} = $prev.=succ;
        }
    }
    nqp::p6bindattrinvres(
      nqp::create(Map),Map,'$!storage',nqp::getattr(%res,Map,'$!storage')
    )
}

Metamodel::EnumHOW.set_composalizer(-> $type, $name, %enum_values {
    my Mu $r := Metamodel::ParametricRoleHOW.new_type(:name($name));
    $r.^add_attribute(Attribute.new(
        :name('$!' ~ $name), :type(nqp::decont($type)),
        :has_accessor(1), :package($r)));
    for %enum_values.kv -> $key, $value {
        my $meth = method () { self."$name"() == $value }
        $meth.set_name($key);
        $r.^add_method($key, $meth);
    }
    $r.^set_body_block( -> |c {nqp::list($r,nqp::hash('$?CLASS',c<$?CLASS>))});
    $r.^compose;
    $r
});

# vim: ft=perl6 expandtab sw=4
