# A SlippyIterator is one that comes with some infrastructure for handling
# flattening a received Slip into its own stream of values.
my role SlippyIterator does Iterator {
    # Flat set to non-zero if the iterator is currently consuming a Slip.
    has int $!slipping;

    # The current Slip we're iterating.
    has $!slip-iter;

    method start-slip(Slip:D $slip) {
        $!slipping = 1;
        $!slip-iter := $slip.iterator;
        self.slip-one()
    }

    method slip-one() {
        my \result = $!slip-iter.pull-one;
        if result =:= IterationEnd {
            $!slipping = 0;
            $!slip-iter := Mu;
        }
        result
    }
}

# vim: ft=perl6 expandtab sw=4
