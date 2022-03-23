class Distribution::Hash does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix --> Nil) { }
    method new($hash, :$prefix) { self.bless(:meta($hash), :$prefix) }
    method meta { $!meta }
    method raku {
        self.^name ~ ".new({$!meta.raku}, prefix => {$!prefix.raku})";
    }
}

# vim: expandtab shiftwidth=4
