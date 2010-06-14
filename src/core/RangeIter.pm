class RangeIter is Iterator {
    has $!value;
    has $!max;
    has $!excludes_max;
    has $!nextIter;

    method reify() {
        return ($!value,) if $!value ~~ EMPTY;
        unless $!nextIter.defined || $!nextIter ~~ EMPTY {
            my $s = $!value.succ;
            $!nextIter =
                $s before $!max || (! $!excludes_max && !($s after $!max))
                ?? RangeIter.new( :value($s),
                                :max($!max), 
                                :excludes_max($!excludes_max) )
                !! EMPTY;
        }
        $!value, $!nextIter;
    }
}

