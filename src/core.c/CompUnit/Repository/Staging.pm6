class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;
    has CompUnit::Repository $!parent;

    submethod TWEAK (--> Nil) {
       $!parent = self.next-repo;
       self.register-name;
    }

    method register-name(CompUnit::Repository::Staging:D:) {
        CompUnit::RepositoryRegistry.register-name($!name, self);
    }

    method unregister-name(CompUnit::Repository::Staging:D: --> Nil) {
        CompUnit::RepositoryRegistry.register-name($!name, $!parent);
    }


    method short-id(--> Str:D) { 'staging' }

    method path-spec(CompUnit::Repository::Staging:D: --> Str:D) {
        self.^name ~ '#name(' ~ $!name ~ ')#' ~ self.prefix.absolute
    }

    method source-file(CompUnit::Repository::Staging:D:
      Str:D $name
    --> IO::Path) {
        my $file := self.prefix.add($name);
        $file.e ?? $file !! $!parent.source-file($name)
    }

    method resource(CompUnit::Repository::Staging:D:
      $dist-id, $key
    ) {
        # check if the dist is installed here
        try self.distribution($dist-id)
          # we have the dist, so it's safe to access the resource the normal way
          ?? callsame()
          # lookup failed, so it's probably not installed here but in the parent
          !! $!parent.resource($dist-id, $key)
    }

    method remove-artifacts(CompUnit::Repository::Staging:D: --> Nil) {
        my $io := self.prefix;
        $io.child($_).unlink for <
          version repo.lock precomp/.lock
        >;
    }

    method deploy(CompUnit::Repository::Staging:D: --> Nil) {
        my $from    := self.prefix.absolute;
        my $relpath := $from.chars;
        my $to      := $!parent.prefix;

        for Rakudo::Internals.DIR-RECURSE($from) -> $path {
            my $destination := $to.add($path.substr($relpath));
            $destination.parent.mkdir;
            $path.IO.copy: $destination;
        }
    }

    sub self-destruct(IO::Path:D $io --> Nil) {
        .d ?? self-destruct($_) !! .unlink for $io.dir;
        $io.rmdir;
    }
    method self-destruct(CompUnit::Repository::Staging:D: --> Nil) {
        my $prefix = self.prefix;
        return unless $prefix.d;
        self-destruct $prefix;
    }
}

# vim: expandtab shiftwidth=4
