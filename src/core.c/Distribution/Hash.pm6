class Distribution::Hash does Distribution::Locally {
    has $.meta is built(:bind);

    method new($meta, :$prefix) { self.bless(:$meta, :$prefix) }
    multi method raku(Distribution::Hash:D:) {
        self.^name ~ ".new($!meta.raku(), prefix => $!prefix.raku())";
    }
}

# vim: expandtab shiftwidth=4
