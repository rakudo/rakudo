augment class Any {
    method ACCEPTS($topic) {
        self === $topic
    }

    multi method Str() {
        sprintf '%s<0x%x>', self.WHAT, self.WHERE;
    }
}

# vim: ft=perl6
