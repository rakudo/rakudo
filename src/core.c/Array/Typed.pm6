my role Array::Typed[::TValue] does Positional[TValue] {

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
          ContainerDescriptor.new(:of(TValue), :default(TValue))
        );
        list
    }

    # must have a proto here to hide the candidates in Array
    # otherwise we could bind any value to the Array
    proto method BIND-POS(|) {*}

    # these BIND-POSses are identical to Array's, except for bindval
    multi method BIND-POS(Array:D: int $pos, TValue \bindval) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..^Inf>)),
          nqp::stmts(
            nqp::if(
              nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::isge_i(
                  $pos,nqp::elems(nqp::getattr(self,List,'$!reified'))
                ) && nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
                nqp::getattr(self,List,'$!todo').reify-at-least(
                  nqp::add_i($pos,1)),
              ),
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
            ),
            nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,bindval)
          )
        )
    }
    # because this is a very hot path, we copied the code from the int candidate
    multi method BIND-POS(Array:D: Int:D $pos, TValue \bindval) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..^Inf>)),
          nqp::stmts(
            nqp::if(
              nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::isge_i(
                  $pos,nqp::elems(nqp::getattr(self,List,'$!reified'))
                ) && nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
                nqp::getattr(self,List,'$!todo').reify-at-least(
                  nqp::add_i($pos,1)),
              ),
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
            ),
            nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,bindval)
          )
        )
    }

    multi method raku(::?CLASS:D:) {
        my $type := nqp::getattr(self,Array,'$!descriptor').of.^name;
        my $raku := self.map({
            nqp::isconcrete($_) ?? .raku(:arglist) !! $type
        }).join(', ');
        'Array[' ~ $type ~ '].new(' ~ $raku ~ ')'
    }
}

# vim: expandtab shiftwidth=4
