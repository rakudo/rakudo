my class StrDistance is Cool {
    has Str $.before;
    has Str $.after;
    has Int $!distance;

    method Bool() {
        $.before ne $.after
    }

    method Numeric() {
        self.Int
    }

    method Int() {
        $!distance //= do {
            my @s = *, $.before.comb;
            my @t = *, $.after.comb;
            my @d;
            @d[$_][ 0] = $_ for ^@s.end;
            @d[ 0][$_] = $_ for ^@t.end;

            for 1..@s.end X 1..@t.end -> $i, $j {
                @d[$i][$j] = @s[$i] eq @t[$j]
                    ??   @d[$i-1][$j-1]    # No operation required when eq
                    !! ( @d[$i-1][$j  ],   # Deletion
                         @d[$i  ][$j-1],   # Insertion
                         @d[$i-1][$j-1],   # Substitution
                       ).min + 1;
            }

            @d[*-1][*-1];
        }
    }

    method ACCEPTS(StrDistance:D:, Mu \other) {
        self
    }
}
