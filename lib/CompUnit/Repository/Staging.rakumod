class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;
    has CompUnit::Repository $!parent;

    submethod BUILD(Str :$!name --> Nil) {
        $!parent = CompUnit::RepositoryRegistry.repository-for-name($!name);
        note "$!parent.prefix() for $!name";
        CompUnit::RepositoryRegistry.register-name($!name, self);
    }

    method short-id() { 'staging' }

    method name(--> Str) {
        $!name
    }

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
}

# vim: expandtab shiftwidth=4
