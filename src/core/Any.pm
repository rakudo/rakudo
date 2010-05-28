augment class Any {
    method ACCEPTS($topic) {
        self === $topic
    }

    multi method Str() {
        sprintf '%s<0x%x>', self.WHAT, self.WHERE;
    }

    method Numeric() {
        die "Can't take numeric value for object of type $.WHAT.perl()" if $.defined;
        # fail "Use of uninitialized value in numeric context";
        note "Use of uninitialized value in numeric context";
        0;
    }
}

# vim: ft=perl6
