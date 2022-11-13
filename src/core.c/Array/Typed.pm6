my role Array::Typed[::TValue] does Positional[TValue] {
    method !out-of-range(int $got) {
        X::OutOfRange.new(:what($*INDEX // 'Index'),:$got,:range<0..^Inf>).Failure
    }

    # must have a proto here to hide the candidates in Array
    # otherwise we could bind any value to the Array
    proto method BIND-POS(|) {*}

    # these BIND-POSses are identical to Array's, except for bindval
    multi method BIND-POS(Array:D: uint $pos, TValue \bindval) is raw {
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
    multi method BIND-POS(Array:D: Int:D $pos, TValue \bindval) is raw {
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

    multi method raku(::?CLASS:D:) {
        my $type := (try TValue.raku)
          // nqp::getattr(self,Array,'$!descriptor').of.^name;
        my $raku := self.map({
            nqp::isconcrete($_) ?? .raku(:arglist) !! $type
        }).join(', ');
        'Array[' ~ $type ~ '].new(' ~ $raku ~ ')'
    }

    method rub(::?CLASS: --> ::?CLASS:D) {
        nqp::if(
          nqp::isconcrete(self),
          (callsame),
          nqp::p6bindattrinvres((callsame),Array,'$!descriptor',
            (ContainerDescriptor.new: :of(TValue), :default(TValue))))
    }
}

# vim: expandtab shiftwidth=4
