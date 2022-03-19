class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;
    has CompUnit::Repository $!parent;

    submethod BUILD(Str :$!name --> Nil) {
        $!parent = CompUnit::RepositoryRegistry.repository-for-name($!name);
        CompUnit::RepositoryRegistry.register-name($!name, self);
    }

    method short-id() { 'staging' }

    method path-spec(CompUnit::Repository::Staging:D:) {
        self.^name ~ '#name(' ~ $!name ~ ')#' ~ $.prefix.absolute;
    }

    method source-file(Str $name --> IO::Path) {
        my $file := $.prefix.add($name);
        $file.e ?? $file !! $!parent.source-file($name)
    }

    method resource($dist-id, $key) {
        # check if the dist is installed here
        try self.distribution($dist-id)
          # we have the dist, so it's safe to access the resource the normal way
          ?? callsame()
          # lookup failed, so it's probably not installed here
          !! $!parent.resource($dist-id, $key)
    }

    sub files($io) {
        $io.dir.map: {
            dd $_;
        }
    }

    method deploy() {
        my $from    := $.prefix.absolute;
        my $relpath := $from.chars;
        my $to      := $!parent.prefix;

        for Rakudo::Internals.DIR-RECURSE($from) -> $path {
            my $destination := $to.add($path.substr($relpath));
            $destination.parent.mkdir;
            $destination.spurt: $path.IO.slurp(:bin);
        }
    }
}

# vim: expandtab shiftwidth=4
