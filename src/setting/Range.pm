class Range is also {
    has $.by = 1;
    has $.from;
    has $.from_exclusive = Bool::False;
    has $.to;
    has $.to_exclusive = Bool::False;

    our Bool multi method ACCEPTS(Range $topic) {
        ?(($.from == $topic.from) && ($.to == $topic.to) &&
          ($.from_exclusive == $topic.from_exclusive) &&
          ($.to_exclusive == $topic.from_exclusive) &&
          ($.by == $topic.by))
    }

    our Bool multi method ACCEPTS($topic) {
        ?(self!from_test($topic) && self!to_test($topic))
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
        if $.by == 1 {
            [~]
                $.from.perl,
                ("^" if $.from_exclusive),
                "..",
                ("^" if $.to_exclusive),
                $.to.perl;
        } else {
            'Range.new('
             ~ join(', ', 
                    'from => '           ~ $.from.perl,
                    'to => '             ~ $.to.perl,
                    'by => '             ~ $.by.perl,
                    'from_exclusive => ' ~ $.from_exclusive.perl,
                    'to_exclusive => '   ~ $.to_exclusive.perl,
                    )
             ~ ')'
        }
    }

    our #(Range) multi method reverse() {
        # XXX Should eventually return a reversed Range.
        @.list.reverse;
    }

    our #(Bool) multi method true() {
        # XXX For some reason, simply ?-ing what follows does not fix the
        # return type to Bool. Needs investigating...
        self!to_test($.from_exclusive ?? ++($.from.clone) !! $.from)
    }

    our Str multi method Str() {
        ~$.list
    }
}
