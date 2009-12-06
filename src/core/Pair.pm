augment class Pair {
    method value() {
        $!value
    }

    multi method perl() {
        # $.key.perl ~ ' => ' ~ $.value.perl;
        "Pair.new(:key({$.key.perl}), :value({$.value.perl}))";
    }

    # I don't see anything in the spec about what
    # this should do.  This is doing the same thing
    # that ~Pair did in old Rakudo master, which didn't
    # seem to have Pair.Str.
    multi method Str() {
        "$.key\t$.value";
    }
}
