class Range is also {
    has $.by = 1;
    has $.from;
    has $.from_exclusive = Bool::False;
    has $.to;
    has $.to_exclusive = Bool::False;

    our Bool multi method ACCEPTS(Range $topic) {
        ($.from == $topic.from) && ($.to == $topic.to) &&
        ($.from_exclusive == $topic.from_exclusive) &&
        ($.to_exclusive == $topic.from_exclusive) &&
        ($.by == $topic.by)
    }

    our Bool multi method ACCEPTS($topic) {
        self!from_test($topic) && self!to_test($topic)
    }

    our Range multi method clone() {
        Range.new(:from($.from), :from_exclusive($.from_exclusive),
                  :to($.to), :to_exclusive($.to_exclusive),
                  :by($.by))
    }

    our Range multi method iterator() {
        $.clone
    }

    multi method max() {
        $.to
    }

    multi method min() {
        $.from
    }

    multi method minmax() {
        ($.from, $.to)
    }

    # TODO: Add support for the :by(..) adverbial modifier.
    our Str multi method perl() {
        [~] gather {
            take $.from.perl;
            take "^" if $.from_exclusive;
            take "..";
            take "^" if $.to_exclusive;
            take $.to.perl
        }
    }

    our Range multi method reverse() {
        Range.new(:from($.to), :from_exclusive($.to_exclusive),
                  :to($.from), :to_exclusive($.from_exclusive),
                  :by(-$.by))
    }

    our Bool multi method true() {
        self!to_test($.from_exclusive ?? ++($.from.clone) !! $.from)
    }

    our Str multi method Str() {
        $.list
    }
}
