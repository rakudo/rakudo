augment class Str {
    multi method comb(Str:D: Pair:D $what, $limit = *, :$partial) {
        my int $size = $what.key;
        my int $step = $size + $what.value;
        $step = 1 if $step < 1;
        $size <= 1 && (nqp::istype($limit,Whatever) || $limit == Inf)
          ?? self.comb
          !! Seq.new:
               Rakudo::Iterator.NGrams: self, $size, $limit, $step, $partial

    }
}

# vim: expandtab shiftwidth=4
