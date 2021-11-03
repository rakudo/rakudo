class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;

    submethod BUILD(Str :$!name --> Nil) {
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
        $file.e ?? $file !! self.next-repo.source-file($name)
    }
}

# vim: expandtab shiftwidth=4
