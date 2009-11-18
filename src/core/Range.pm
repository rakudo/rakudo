class Range {
    has $.min;
    has $.excludes_min = Bool::False;
    has $.max;
    has $.excludes_max = Bool::False;

    multi method new($min,
                     $max,
                     Bool $excludes_min = Bool::False,
                     Bool $excludes_max = Bool::False) {
        self.bless(*, :min($min),
                      :max($max),
                      :excludes_min($excludes_min),
                      :excludes_max($excludes_max));
    }

    # our Bool multi method ACCEPTS(Range $topic) {
    #     ?(($.min == $topic.min) && ($.max == $topic.max) &&
    #       ($.excludes_min == $topic.excludes_min) &&
    #       ($.excludes_max == $topic.excludes_min)
    # }

    our Bool multi method ACCEPTS($topic) {
        ?(self!min_test($topic) && self!max_test($topic))
    }

    # our Range multi method iterator() {
    #     $.clone
    # }

    multi method minmax() {
        ($.min, $.max)
    }

    # Beautiful implementation which does not work yet in ng
    # our Str multi method perl() {
    #     [~]
    #         $.min.perl,
    #         ("^" if $.excludes_min),
    #         "..",
    #         ("^" if $.excludes_max),
    #         $.max.perl;
    # }

    # CHEAT: This ignores excludes_min and excludes_max
    our Str multi method perl() {
        $.min.perl ~ ".." ~ $.max.perl;
    }

    our Str multi method Str() {
        ~$.list
    }
}

our multi sub infix:<..>($a, $b) {
    Range.new($a, $b);
}

our multi sub infix:<^..>($a, $b) {
    Range.new($a, $b, Bool::True, Bool::False);
}

our multi sub infix:<..^>($a, $b) {
    Range.new($a, $b, Bool::False, Bool::True);
}

our multi sub infix:<^..^>($a, $b) {
    Range.new($a, $b, Bool::True, Bool::True);
}
