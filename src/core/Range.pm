class Range is Iterable {
    has $.min;
    has $.max;

    proto method new(|$) { * }
    multi method new($min, $max) {
        self.CREATE.BUILD($min, $max)
    }
    multi method new($min, Whatever $max) {
        self.CREATE.BUILD($min, $Inf)
    }

    method BUILD($min, $max) {
        $!min = $min;
        $!max = $max;
        self;
    }

    method iterator() { RangeIter.new(:value($.min), :max($.max)) }

    multi method gist(Range:D:) { self.perl }
    multi method perl(Range:D:) { $.min ~ '..' ~ $.max }
}


sub infix:<..>($min, $max) { Range.new($min, $max) }


