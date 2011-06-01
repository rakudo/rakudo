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

    multi method new($min, Whatever $max,
                     Bool :$excludes_min = Bool::False,
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :$min, :max(+Inf), :$excludes_min, :$excludes_max);
    }

    multi method new(Whatever $min, $max,
                     Bool :$excludes_min = Bool::False,
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :min(-Inf), :$max, :$excludes_min, :$excludes_max);
    }

    multi method new(Whatever $min, Whatever $max,
                     Bool :$excludes_min = Bool::False,
                     Bool :$excludes_max = Bool::False) {
        fail "*..* is not a valid range";
    }

    multi method new($min, $max,
                     Bool :$excludes_min = Bool::False,
                     Bool :$excludes_max = Bool::False) {
        ($min ~~ Real or $max ~~ Real)
        ?? self.bless(*, :min($min.Numeric), :max($max.Numeric), :$excludes_min, :$excludes_max)
        !! nextsame;
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

    multi method pick() {
        self.roll;
    }

    multi method pick(1) {
        self.roll;
    }

    multi method pick(Whatever) {
        self.pick(Inf);
    }

    multi method roll() {
        nextsame unless $.min.isa(Int) and $.max.isa(Int);
        my $least = $.excludes_min ?? $.min + 1 !! $.min;
        my $elems = 1 + ($.excludes_max ?? $.max - 1 !! $.max) - $least;
        $elems ?? ($least + $elems.rand.floor) !! Any;
    }

    multi method roll($num) {
        nextsame unless $.min.isa(Int) and $.max.isa(Int);
        return self.roll if $num == 1;
        (^$num).map: { self.roll }
    }

    multi method roll(Whatever) {
        self.roll(Inf);
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
