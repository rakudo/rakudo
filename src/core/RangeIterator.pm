

class RangeIterator {
    has $!value;
    has $!max;
    has $!excludes_max;

    multi method new(Range $r) {
        $!value        = $r.min;
        $!max          = $r.max;
        $!excludes_max = $r.excludes_max;
        self.get if $r.excludes_min;
    }

    method get() {
        my $return = $!value;
        return IterDone if $return after $!max 
                           || $!excludes_max && !($return before $!max);
        $!value .= succ;
        $return;
    }
}
