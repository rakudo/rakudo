class Range is Iterable does Positional {
    has $.min;
    has $.excludes_min = Bool::False;
    has $.max;
    has $.excludes_max = Bool::False;

    multi method new(::T $min, T $max, 
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :$min, :$max, :$excludes_min, :$excludes_max);
    }

    multi method new(Real $min, $max,
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :$min, :max($max.Numeric), :$excludes_min, :$excludes_max);
    }

    multi method new($min, Real $max,
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :min($min.Numeric), :$max, :$excludes_min, :$excludes_max);
    }

    multi method new(Whatever $min, $max,
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.new(-Inf, $max, :$excludes_min, :$excludes_max);
    }

    multi method new($min, Whatever $max,
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.new($min, +Inf, :$excludes_min, :$excludes_max);
    }

    multi method new(Whatever $min, Whatever $max,
                     Bool :$excludes_min = Bool::False, 
                     Bool :$excludes_max = Bool::False) {
        self.new(-Inf, +Inf, :$excludes_min, :$excludes_max);
    }

    multi method bounds() { ($.min, $.max) }
    multi method from() { $.min; }
    multi method to() { $.max; }

    multi method iterator() {
        RangeIter.new(:value($!excludes_min ?? $!min.succ !! $!min), 
                      :$!max, :$!excludes_max);
    }

    our Str multi method perl() {
        ( $.min.perl, 
          ('^' if $.excludes_min), 
          '..', 
          ('^' if $.excludes_max), 
          $.max.perl
        ).join('');
    }

    our Bool multi method ACCEPTS(Range $topic) {
        ?(($.min eqv $topic.min)
          && ($.max eqv $topic.max)
          && ($.excludes_min == $topic.excludes_min)
          && ($.excludes_max == $topic.excludes_min));
    }

    our Bool multi method ACCEPTS($topic) {
        ?(self!min_test($topic) && self!max_test($topic))
    }

    multi method postcircumfix:<[ ]>(\$parcel) { self.Seq[$parcel]; }

    my Bool multi method !max_test($topic) {
        $topic before $.max || (!$.excludes_max && !($topic after $.max));
    }

    my Bool multi method !min_test($topic) {
        $.min before $topic || (!$.excludes_min && !($.min after $topic));
    }
    
}


our multi sub infix:<..>($min, $max) {
    Range.new($min, $max);
}

our multi sub infix:<^..>($min, $max) {
    Range.new($min, $max, :excludes_min(Bool::True));
}

our multi sub infix:<..^>($min, $max) {
    Range.new($min, $max, :excludes_max(Bool::True));
}

our multi sub infix:<^..^>($min, $max) {
    Range.new($min, $max, :excludes_min(Bool::True), :excludes_max(Bool::True));
}

our multi sub prefix:<^>($max) {
    0..^+$max;
}
