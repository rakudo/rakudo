class Range is Iterable does Positional {
    has $.min;
    has $.excludes_min = Bool::False;
    has $.max;
    has $.excludes_max = Bool::False;

    multi method new($min,
                     $max,
                     Bool :$excludes_min = Bool::False,
                     Bool :$excludes_max = Bool::False) {
        self.bless(*, :min($min ~~ Whatever ?? -Inf !! $min),
                      :max($max ~~ Whatever ?? Inf !! $max),
                      :excludes_min($excludes_min),
                      :excludes_max($excludes_max));
    }

    our Bool multi method ACCEPTS(Range $topic) {
        ?(($.min == $topic.min)
          && ($.max == $topic.max)
          && ($.excludes_min == $topic.excludes_min)
          && ($.excludes_max == $topic.excludes_min));
    }

    our method iterator() {
        my $start = $.min;
        $start .= succ if $.excludes_min;
        RangeIter.new( :value( self!max_test($start) ?? $start !! EMPTY ),
                       :max($.max), 
                       :excludes_max($.excludes_max));
    }

    my Bool multi method !min_test($topic) {
        $.min == -Inf || $.min before $topic || (!$.excludes_min && !($.min after $topic));
    }

    my Bool multi method !max_test($topic) {
        $.max == Inf || $topic before $.max || (!$.excludes_max && !($topic after $.max));
    }

    our Bool multi method ACCEPTS($topic) {
        ?(self!min_test($topic) && self!max_test($topic))
    }

    multi method bounds() {
        ($.min, $.max)
    }

    multi method from() { $.min; }
    multi method to() { $.max; }

    # Beautiful implementation which does not work yet in ng
    # our Str multi method perl() {
    #     [~]
    #         $.min.perl,
    #         ("^" if $.excludes_min),
    #         "..",
    #         ("^" if $.excludes_max),
    #         $.max.perl;
    # }

    our Str multi method perl() {
        my $min = $.min ~~ ::Whatever ?? "*" !! $.min.perl;
        my $emin = $.excludes_min ?? "^" !! "";
        my $max = $.max ~~ ::Whatever ?? "*" !! $.max.perl;
        my $emax = $.excludes_max ?? "^" !! "";
        $min ~ $emin ~ ".." ~ $emax ~ $max;
    }

    multi method fmt($format = '%s', $seperator = ' ') {
        self.map({ .fmt($format)}).join($seperator);
    }

    multi method postcircumfix:<[ ]>(Int $index) { self.Seq[$index] }
    multi method postcircumfix:<[ ]>(@slice)     { self.Seq[@slice] }
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
