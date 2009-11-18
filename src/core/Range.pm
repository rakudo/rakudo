class Range {
    has $.from;
    has $.from_exclusive = Bool::False;
    has $.to;
    has $.to_exclusive = Bool::False;

    # our Bool multi method ACCEPTS(Range $topic) {
    #     ?(($.from == $topic.from) && ($.to == $topic.to) &&
    #       ($.from_exclusive == $topic.from_exclusive) &&
    #       ($.to_exclusive == $topic.from_exclusive)
    # }

    our Bool multi method ACCEPTS($topic) {
        ?(self!from_test($topic) && self!to_test($topic))
    }

    # our Range multi method iterator() {
    #     $.clone
    # }

    multi method max() {
        $.to
    }

    multi method min() {
        $.from
    }

    multi method minmax() {
        ($.from, $.to)
    }

    # our Str multi method perl() {
    #     [~]
    #         $.from.perl,
    #         ("^" if $.from_exclusive),
    #         "..",
    #         ("^" if $.to_exclusive),
    #         $.to.perl;
    # }

    our Str multi method Str() {
        ~$.list
    }
}
