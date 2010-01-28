

class RangeIterator is Iterator {
    has $!value;
    has $!max;
    has $!excludes_max;

    multi method new(Range $r) {
        self.bless(*, :value($r.excludes_min ?? $r.min.succ !! $r.min),
                      :max($r.max),
                      :excludes_max($r.excludes_max));
    }
    
    method get() {
        my $current = $!value;
        if $current after $!max
           || $!excludes_max && !($current before $!max) {
            return IterDone;
        }
        $!value .= succ;
        $current;
    }
}
