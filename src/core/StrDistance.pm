my class StrDistance is Cool {
    has Str $.before;
    has Str $.after;
    has Int $!distance;

    submethod BUILD(Str() :$!before, :$!after --> Nil) { }

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

            for flat 1..@s.end X 1..@t.end -> $i, $j {
                @d[$i][$j] = @s[$i] eq @t[$j]
                    ??   @d[$i-1][$j-1]    # No operation required when eq
                    !! ( @d[$i-1][$j  ],   # Deletion
                         @d[$i  ][$j-1],   # Insertion
                         @d[$i-1][$j-1],   # Substitution
                       ).min + 1;
            }

            @d.tail.tail;
        }
    }
}
