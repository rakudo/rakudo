class Distribution::Hash does Distribution::Locally {
    has $.meta is built(:bind);

    method new($meta, :$prefix) { self.bless(:$meta, :$prefix) }
    method raku {
        self.^name ~ ".new($!meta.raku(), prefix => $!prefix.raku())";
    }
}

# vim: expandtab shiftwidth=4
