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

    multi method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format)}).join($separator);
    }

    multi method postcircumfix:<[ ]>(Int $index) { self.Seq[$index] }
    multi method postcircumfix:<[ ]>(@slice)     { self.Seq[@slice] }

    class InfiniteIntRangeIter is Iterator {
        has $!value;
        has $!nextIter;

        method infinite() { True; }

        method reify() {
            unless $!nextIter.defined {
                $!nextIter = InfiniteIntRangeIter.new( :value($!value + 8) );
            }
            $!value, $!value + 1, $!value + 2, $!value + 3,
            $!value + 4, $!value + 5, $!value + 6, $!value + 7, $!nextIter;
        }
    }

    class FiniteIntRangeQuadIter is Iterator {
        has $!value;
        has $!max;
        has $!nextIter;

        method infinite() { False }

        method reify() {
            return ($!value,) if $!value ~~ EMPTY;
            unless $!nextIter.defined || $!nextIter ~~ EMPTY {
                if $!value != $!max {
                    my $s = $!value + 4;
                    $!nextIter = $s < $!max
                                    ?? FiniteIntRangeQuadIter.new( :value($s),
                                                                   :max($!max) )
                                    !! EMPTY;
                } else {
                    $!nextIter = EMPTY;
                }
            }
            $!value, $!value + 1, $!value + 2, $!value + 3, $!nextIter;
        }
    }

    class ConsIter is Iterator {
        has $!value-parcel;
        has $!nextIter;

        method infinite() { $!nextIter ~~ EMPTY ?? False !! $!nextIter.infinite; }

        method reify() {
            &infix:<,>(|$!value-parcel, $!nextIter);
        }
    }

    our method iterator() {
        if $.min ~~ Int && $.max == Inf {
                return InfiniteIntRangeIter.new( :value($!excludes_min ?? $.min + 1 !! $.min) );
        }

        my $start = $.min;
        $start .= succ if $.excludes_min;

        if $.min ~~ Int && $.max ~~ Int {
            my $end = $.max;
            $end .= pred if $.excludes_max;
            return EMPTY if $start > $end;

            my $mod = ($end - $start + 1) % 4;
            if $mod == 0 {
                FiniteIntRangeQuadIter.new(:value($start), :max($end));
            } else {
                my $parcel := pir::new__Ps('Parcel');
                my $i;
                loop ($i = 0; $i < $mod; $i++) {
                     pir::push($parcel, $start + $i);
                }

                my $next-iter = $start + $mod < $end
                    ?? FiniteIntRangeQuadIter.new(:value($start + $mod),
                                                  :max($end))
                    !! EMPTY;
                ConsIter.new(:value-parcel($parcel),
                             :nextIter($next-iter));
            }
        } else {
            RangeIter.new( :value( self!max_test($start) ?? $start !! EMPTY ),
                           :max($.max),
                           :excludes_max($.excludes_max));
        }
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
