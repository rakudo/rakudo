class Version is List {
    has Bool $.plus = False;
    method new(*@positional, :$plus) {
        my $new := nextwith(@positional);
        nqp::bindattr($new, Version, '$!plus', so $plus);
        $new;
    }
    multi method Str(Version:D:) {
        'v' ~ self.join('.');
    }
    multi method gist(Version:D:) { self.Str }
    multi method perl(Version:D:) {
        self.^name ~ '.new(' ~ self.List::perl ~ ', :plus(' ~ $!plus.perl ~ '))';

    }
}
