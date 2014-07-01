my class Method { # declared in BOOTSTRAP
    # class Method is Routine { ... }

    multi method gist(Method:D:) { self.name }
}

multi sub trait_mod:<is>(Method $m, :$cached!) {
    my %cache;
    $m.wrap(-> $self, |c {
        my $key := $self.WHICH ~ '|' ~ c.gist;
        %cache{$key}:exists
          ?? %cache{$key}
          !! (%cache{$key} = callsame);
    });
}

# vim: ft=perl6 expandtab sw=4
