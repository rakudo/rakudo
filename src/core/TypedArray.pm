# this is actually part of the Array class

    my role TypedArray[::TValue] does Positional[TValue] {
        proto method new(|) { * }
        multi method new(**@values is raw, :$shape) {
            self!new-internal(@values, $shape);
        }
        multi method new(\values, :$shape) {
            self!new-internal(values, $shape);
        }

        method !new-internal(\values, \shape) {
            set-shape(
              nqp::p6bindattrinvres(
                nqp::create(self),
                Array,
                '$!descriptor',
                Perl6::Metamodel::ContainerDescriptor.new(
                  :of(TValue), :rw(1), :default(TValue))
              ),
              values,
              shape
            )
        }

        proto method BIND-POS(|) { * }
        multi method BIND-POS(Array:D: Int $pos, TValue \bindval) is raw {
            my int $ipos = $pos;
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval)
        }
        multi method BIND-POS(Array:D: int $pos, TValue \bindval) is raw {
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval)
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
