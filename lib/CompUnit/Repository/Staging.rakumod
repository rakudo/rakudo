class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;
    has CompUnit::Repository $!parent;

    submethod TWEAK(--> Nil) {
        $!parent = CompUnit::RepositoryRegistry.repository-for-name($!name);
        CompUnit::RepositoryRegistry.register-name($!name, self);
    }

    method short-id() { 'staging' }

    method path-spec(CompUnit::Repository::Staging:D:) {
        self.^name ~ '#name(' ~ $!name ~ ')#' ~ $.prefix.absolute;
    }

    method source-file(Str $name --> IO::Path) {
        my $file = self.prefix.add($name);
        $file.e ?? $file !! $!parent.source-file($name)
    }

    method resource($dist-id, $key) {
        try self.distribution($dist-id) # check if the dist is installed here
            ?? callsame() # we have the dist, so it's safe to access the resource the normal way
            !! $!parent.resource($dist-id, $key) # lookup failed, so it's probably not installed here
    }

    method deploy() {
        my $from    := $.prefix.absolute;
        my $relpath := $from.chars;
        my $to      := $!parent.prefix;

        for Rakudo::Internals.DIR-RECURSE($from) -> $path {
            my $destination := $to.add($path.substr($relpath));
            $destination.parent.mkdir;
            $path.IO.copy: $destination;
        }
    }

    sub self-destruct($io) {
        .d ?? self-destruct($_) !! .unlink for $io.dir;
        $io.rmdir;
    }
    method self-destruct() { self-destruct($.prefix) }
}

# vim: expandtab shiftwidth=4
