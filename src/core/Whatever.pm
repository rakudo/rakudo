my class Whatever {
    multi method ACCEPTS(Whatever:D: $topic) { True }
    multi method perl(Whatever:D:) { '*' }
}

my class HyperWhatever {
    multi method ACCEPTS(HyperWhatever:D: $topic) { True }
    multi method perl(HyperWhatever:D:) { '**' }
}

sub HYPERWHATEVER (&c) { sub (*@_) { map &c, @_ } }

# vim: ft=perl6 expandtab sw=4
