class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;

    submethod BUILD(Str :$!name --> Nil) { }

    method short-id() { 'staging' }

    method name(--> Str) {
        $!name
    }
    method path-spec(CompUnit::Repository::Staging:D:) {
        self.short-id ~ '#name(' ~ $!name ~ ')#' ~ $.prefix.abspath;
    }
    method source-file(Str $name --> IO::Path) {
        my $file = self.prefix.child($name);
        $file.e ?? $file !! self.next-repo.source-file($name)
    }
}

# vim: ft=perl6
