my class X::Enum::NoValue {...};
# Method that we have on enumeration types.
my role Enumeration {
    has $.key;
    has $.value;
    has int $!index;

    method enums() { self.^enum_values.Map }

    multi method kv(::?CLASS:D:) { ($!key, $!value) }
    method pair(::?CLASS:D:) { $!key => $!value }

    multi method gist(::?CLASS:D:) { $!key                     }
    multi method raku(::?CLASS:D:) { self.^name ~ '::' ~ $!key }

    multi method pick(::?CLASS:U:)       { self.^enum_value_list.pick     }
    multi method pick(::?CLASS:U: \n)    { self.^enum_value_list.pick(n)  }
    multi method pick(::?CLASS:D: *@pos) { self xx +?( @pos[0] // 1 )     }
    multi method roll(::?CLASS:U:)       { self.^enum_value_list.roll     }
    multi method roll(::?CLASS:U: \n)    { self.^enum_value_list.roll(n)  }
    multi method roll(::?CLASS:D: *@pos) { self xx +?( @pos[0] // 1 )     }

    multi method Numeric(::?CLASS:D:) { $!value.Numeric }
    multi method Int(::?CLASS:D:)     { $!value.Int }
    multi method Real(::?CLASS:D:)    { $!value.Real }

    multi method WHICH(::?CLASS:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(self.^name,nqp::concat("|",$!index)),
          ValueObjAt
        )
    }

    multi method ACCEPTS(::?CLASS:D: ::?CLASS:D \v) { self === v }

    method !FROM-VALUE(Mu \val) {
        my $res := Nil;
        my $dcval := nqp::decont(val);
        # If value is a mixin of enum try to pull out the mixed in value first
        if $dcval.^is_mixin {
            my $attr_name := '$!' ~ self.^name;
            if $dcval.^has_attribute($attr_name) {
                my $mixin_value := nqp::getattr($dcval, $dcval.WHAT, $attr_name);
                return $mixin_value if nqp::istype($mixin_value, ::?CLASS);
            }
        }
        if nqp::istype($dcval, ::?CLASS) {
            $res := $dcval;
        }
        elsif nqp::isconcrete($dcval) {
            $res := self.^enum_from_value($dcval);
        }
        $res // Failure.new(X::Enum::NoValue.new(:type(self.WHAT), :value($dcval)))
    }

    proto method CALL-ME(Mu) {*}
    multi method CALL-ME(Mu \val) { self!FROM-VALUE(val) }

    proto method COERCE(Mu) {*}
    multi method COERCE(Mu \val) { self!FROM-VALUE(val) }

    method pred(::?CLASS:D:) {
        nqp::getattr_i(self,::?CLASS,'$!index')
          ?? nqp::atpos(
               nqp::getattr(self.^enum_value_list,List,'$!reified'),
               nqp::sub_i(nqp::getattr_i(self,::?CLASS,'$!index'),1)
             )
          !! self
    }
    method succ(::?CLASS:D:) {
        my $values := nqp::getattr(self.^enum_value_list,List,'$!reified');
        nqp::islt_i(
          nqp::getattr_i(self,::?CLASS,'$!index'),
          nqp::sub_i(nqp::elems($values),1)
        ) ?? nqp::atpos(
               $values,
               nqp::add_i(nqp::getattr_i(self,::?CLASS,'$!index'),1)
             )
          !! self
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
my role NumericStringyEnumeration {
    multi method Str(::?CLASS:D:) {
        self.key
    }
}

sub ENUM_VALUES(*@args --> Map:D) is implementation-detail {
    my Mu $prev = -1;
    my $res := nqp::hash;

    nqp::istype($_,Pair)
      ?? nqp::bindkey($res, .key, nqp::decont($prev = .value))
      !! nqp::bindkey($res,   $_, nqp::decont($prev = $prev.succ))
      for @args;

    nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$res)
}

Metamodel::EnumHOW.set_composalizer(-> $type, $name, @enum_values {
    my Mu $r := Metamodel::ParametricRoleHOW.new_type(:name($name));
    $r.^add_attribute(Attribute.new(
        :name('$!' ~ $name), :type(nqp::decont($type)),
        :has_accessor(1), :package($r)));
    for @enum_values {
        my $key   = $_.key;
        my $value = $_.value;
        my $meth = method () { self."$name"() == $value }
        $meth.set_name($key);
        $r.^add_method($key, $meth);
    }
    $r.^set_body_block( -> |c {nqp::list($r,nqp::hash('$?CLASS',c<$?CLASS>))});
    $r.^compose;
    $r
});

# We use this one because, for example, Int:D === Int:D, has an optimization
# that simply unboxes the values. That's no good for us, since two different
# Enumeration:Ds could have the same Int:D value.
multi infix:<===> (Enumeration:D \a, Enumeration:D \b --> Bool:D) {
    nqp::hllbool(nqp::eqaddr(nqp::decont(a), nqp::decont(b)))
}

# vim: expandtab shiftwidth=4
