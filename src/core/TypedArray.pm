# this is actually part of the Array class

    my role TypedArray[::TValue] does Positional[TValue] {

        proto method new(|) { * }
        multi method new(:$shape!) {
            set-descriptor(nqp::if(
              nqp::defined($shape),
              set-shape(self,$shape),
              nqp::if(
                Metamodel::EnumHOW.ACCEPTS($shape.HOW),
                set-shape(self,$shape.^elems),
                nqp::create(self)
              )
            ))
        }
        multi method new() {
            set-descriptor(nqp::create(self))
        }
        multi method new(\values, :$shape!) {
            set-descriptor(nqp::if(
              nqp::defined($shape),
              set-shape(self,$shape),
              nqp::if(
                Metamodel::EnumHOW.ACCEPTS($shape.HOW),
                set-shape(self,$shape.^elems),
                nqp::create(self)
              )
            )).STORE(values)
        }
        multi method new(\values) {
            set-descriptor(nqp::create(self)).STORE(values)
        }
        multi method new(**@values is raw, :$shape!) {
            set-descriptor(nqp::if(
              nqp::defined($shape),
              set-shape(self,$shape),
              nqp::if(
                Metamodel::EnumHOW.ACCEPTS($shape.HOW),
                set-shape(self,$shape.^elems),
                nqp::create(self)
              )
            )).STORE(@values)
        }
        multi method new(**@values is raw) {
            set-descriptor(nqp::create(self)).STORE(@values)
        }

        sub set-descriptor(\list) is raw {
            nqp::stmts(
              nqp::bindattr(list,Array,'$!descriptor',
                Perl6::Metamodel::ContainerDescriptor.new(
                  :of(TValue), :rw(1), :default(TValue))
              ),
              list
            )
        }

        # must have a proto here to hide the candidates in Array
        # otherwise we could bind any value to the Array
        proto method BIND-POS(|) { * }

        # these BIND-POSses are identical to Array's, except for bindval
        multi method BIND-POS(Array:D: int $pos, TValue \bindval) is raw {
            nqp::if(
              nqp::islt_i($pos,0),
              Failure.new(X::OutOfRange.new(
                :what($*INDEX // 'Index'),:got($pos),:range<0..^Inf>)),
              nqp::stmts(
                nqp::if(
                  nqp::getattr(self,List,'$!reified').DEFINITE,
                  nqp::if(
                    (nqp::isge_i(
                      $pos,nqp::elems(nqp::getattr(self,List,'$!reified')))
                        && nqp::getattr(self,List,'$!todo').DEFINITE),
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
                  nqp::getattr(self,List,'$!reified').DEFINITE,
                  nqp::if(
                    (nqp::isge_i(
                      $pos,nqp::elems(nqp::getattr(self,List,'$!reified')))
                        && nqp::getattr(self,List,'$!todo').DEFINITE),
                    nqp::getattr(self,List,'$!todo').reify-at-least(
                      nqp::add_i($pos,1)),
                  ),
                  nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
                ),
                nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,bindval)
              )
            )
        }

        multi method perl(::?CLASS:D \SELF:) {
            my $args = self.map({ ($_ // TValue).perl(:arglist) }).join(', ');
            'Array[' ~ TValue.perl ~ '].new(' ~ $args ~ ')';
        }
    }
    method ^parameterize(Mu:U \arr, Mu:U \t, |c) {
        if c.elems == 0 {
            my $what := arr.^mixin(TypedArray[t]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        else {
            die "Can only type-constrain Array with [ValueType]"
        }
    }
}

#========== closed down the Array class started in src/core/Array.pm  ==========

# vim: ft=perl6 expandtab sw=4
