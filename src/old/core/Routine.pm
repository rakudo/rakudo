augment class Routine {

=begin item cando

=end item
    method cando(Capture $capture) {
        my @candidates = self.candidates;
        my @can;
        for @candidates -> $c {
            @can.push($c) if $capture ~~ $c.signature;
        }
        @can
    }

=begin item candidates

=end item
    method candidates() {
        @(self)
    }

}

# vim: ft=perl6
