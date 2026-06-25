my role Array::Typed[::CORE'Array'Typed'TValue] does Positional[CORE'Array'Typed'TValue] {

    proto method new(|) {*}
    multi method new(:$shape!) {
        set-descriptor(
          nqp::defined($shape)
            ?? self.set-shape($shape)
            !! Metamodel::EnumHOW.ACCEPTS($shape.HOW)
              ?? self.set-shape($shape.^elems)
              !! nqp::create(self)
        )
    }
    multi method new() {
        set-descriptor(nqp::create(self))
    }
    multi method new(\values, :$shape!) {
        set-descriptor(
          nqp::defined($shape)
            ?? self.set-shape($shape)
            !! Metamodel::EnumHOW.ACCEPTS($shape.HOW)
              ?? self.set-shape($shape.^elems)
              !! nqp::create(self)
        ).STORE(values)
    }
    multi method new(\values) {
        set-descriptor(nqp::create(self)).STORE(values)
    }
    multi method new(**@values is raw, :$shape!) {
        set-descriptor(
          nqp::defined($shape)
            ?? self.set-shape($shape)
            !! Metamodel::EnumHOW.ACCEPTS($shape.HOW)
              ?? self.set-shape($shape.^elems)
              !! nqp::create(self)
        ).STORE(@values)
    }
    multi method new(**@values is raw) {
        set-descriptor(nqp::create(self)).STORE(@values)
    }

    sub set-descriptor(\list) is raw {
        nqp::bindattr(list,Array,'$!descriptor',
          ContainerDescriptor.new(:of(CORE'Array'Typed'TValue), :default(CORE'Array'Typed'TValue))
        );
        list
    }

    method !out-of-range(int $got) {
        X::OutOfRange.new(:what($*INDEX // 'Index'),:$got,:range<0..^Inf>).Failure
    }

    # must have a proto here to hide the candidates in Array
    # otherwise we could bind any value to the Array
    proto method BIND-POS(|) {*}

    # these BIND-POSses are identical to Array's, except for bindval
    multi method BIND-POS(Array:D: uint $pos, CORE'Array'Typed'TValue \bindval) is raw {
        nqp::if(
          nqp::isconcrete(
            my $reified := nqp::getattr(self,List,'$!reified')
          ),
          nqp::if(
            nqp::isge_i($pos,nqp::elems($reified))
              && nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
            nqp::getattr(self,List,'$!todo').reify-at-least(
              nqp::add_i($pos,1)),
          ),
          ($reified := nqp::bindattr(
            self,List,'$!reified',nqp::create(IterationBuffer)
          ))
        );
        nqp::bindpos($reified,$pos,bindval)
    }
    # because this is a very hot path, we copied the code from the int candidate
    multi method BIND-POS(Array:D: Int:D $pos, CORE'Array'Typed'TValue \bindval) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          self!out-of-range($pos),
          nqp::stmts(
            nqp::if(
              nqp::isconcrete(
                my $reified := nqp::getattr(self,List,'$!reified')
              ),
              nqp::if(
                nqp::isge_i($pos,nqp::elems($reified))
                  && nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
                nqp::getattr(self,List,'$!todo').reify-at-least(
                  nqp::add_i($pos,1)
                )
              ),
              ($reified := nqp::bindattr(
                self,List,'$!reified',nqp::create(IterationBuffer)
              ))
            ),
            nqp::bindpos($reified,$pos,bindval)
          )
        )
    }

    method is-generic { nqp::hllbool(callsame() || nqp::istrue(CORE'Array'Typed'TValue.^archetypes.generic)) }

    multi method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment) is raw {
        self.^mro.first({ !(.^is_mixin && .is-generic) }).^parameterize: type-environment.instantiate(CORE'Array'Typed'TValue)
    }
    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment) is raw {
        my \ins-arr = self.WHAT.INSTANTIATE-GENERIC(type-environment);
        my Mu $descr := type-environment.instantiate(nqp::getattr(self, Array, '$!descriptor'));
        nqp::p6bindattrinvres((self.elems ?? ins-arr.new(self) !! ins-arr.new), Array, '$!descriptor', $descr )
    }

    multi method raku(::?CLASS:D:) {
        my $type := (try CORE'Array'Typed'TValue.raku) // nqp::getattr(self,Array,'$!descriptor').of.^name;
        my $raku := self.map({
            nqp::isconcrete($_) ?? .raku(:arglist) !! $type
        }).join(', ');
        'Array[' ~ $type ~ '].new(' ~ $raku ~ ')'
    }
}

# vim: expandtab shiftwidth=4
