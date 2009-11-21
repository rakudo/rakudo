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

    our Bool multi method ACCEPTS(Range $topic) {
        ?(($.min == $topic.min)
          && ($.max == $topic.max)
          && ($.excludes_min == $topic.excludes_min)
          && ($.excludes_max == $topic.excludes_min));
    }

    my Bool multi method !min_test($topic) {
        $.min lt $topic || (!$.excludes_min && $.min eq $topic);
    }

    my Bool multi method !max_test($topic) {
        $topic lt $.max || (!$.excludes_max && $.max eq $topic);
    }

    our Bool multi method ACCEPTS($topic) {
        ?(self!min_test($topic) && self!max_test($topic))
    }

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

    our Str multi method perl() {
        my $emin = $.excludes_min ?? "^" !! "";
        my $emax = $.excludes_max ?? "^" !! "";
        $.min.perl ~ $emin ~ ".." ~ $emax ~ $.max.perl;
    }

    our Str multi method Str() {
        self.perl;
    }
}

our multi sub infix:<..>($min, $max) {
    Range.new($min, $max);
}

our multi sub infix:<^..>($min, $max) {
    Range.new($min, $max, Bool::True, Bool::False);
}

our multi sub infix:<..^>($min, $max) {
    Range.new($min, $max, Bool::False, Bool::True);
}

our multi sub infix:<^..^>($min, $max) {
    Range.new($min, $max, Bool::True, Bool::True);
}

# CHEAT: This is overly broad, but I don't know how to
# limit it to numeric types in ng.
our multi sub prefix:<^>($max) {
    0..^$max;
}

class RangeIterator {
    has $.range;
    has $.current;

    multi method new(Range $range,
                     $current) {
        say "making" ~ $range.perl;
        self.bless(*, :range($range),
                      :current($current));
    }

    multi method get() {
        say "in get";
        $!current <= $!range.max ?? $!current++ !! Nil;
    }
}

augment class Range {
    our Range multi method Iterator() {
        RangeIterator.new(self, $.min);
    }
}
