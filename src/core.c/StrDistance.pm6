my class StrDistance is Cool {
    has Str $.before is built(:bind);
    has Str $.after is built(:bind);
    has Int $!distance;

    method Bool() {
        $.before ne $.after
    }

    method ACCEPTS(StrDistance:D: Mu \a) {
        self
    }

    method Numeric() {
        self.Int
    }

    method Str {
        $.after
    }

    multi method Int(StrDistance:D:) {
        $!distance //= do {
            my @s = *, |$.before.comb;
            my @t = *, |$.after.comb;
            my @d;
            @d[$_][ 0] = $_ for ^@s.end;
            @d[ 0][$_] = $_ for ^@t.end;

            my int $s_elems = @s.elems;
            my int $t_elems = @t.elems;
            loop (my int $i = 1; $i < $s_elems; $i = $i + 1) {
                loop (my int $j = 1; $j < $t_elems; $j = $j + 1) {
                    @d[$i][$j] = @s[$i] eq @t[$j]
                        ??   @d[$i-1][$j-1]    # No operation required when eq
                        !! ( @d[$i-1][$j  ],   # Deletion
                             @d[$i  ][$j-1],   # Insertion
                             @d[$i-1][$j-1],   # Substitution
                           ).min + 1;
                }
            }

            @d.tail.tail;
        }
    }
}

# vim: expandtab shiftwidth=4
