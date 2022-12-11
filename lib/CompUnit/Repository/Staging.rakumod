class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;
    has CompUnit::Repository $!parent;

    submethod TWEAK(--> Nil) {
        $!parent = CompUnit::RepositoryRegistry.repository-for-name($!name);
        CompUnit::RepositoryRegistry.register-name($!name, self);
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
        (try self.distribution($dist-id))
          # we have the dist, so it's safe to access the resource the normal way
          ?? callsame()
          # lookup failed, so it's probably not installed here but in the parent
          !! $!parent.resource($dist-id, $key)
    }

    my sub really-unlink(IO::Path:D $io) {
        # attempt to unlink until succeeds at .1 second intervals
        # needed on Windows because it can easily race and fail there
        if Rakudo::Internals.IS-WIN {
            for ^10 {
                .throw without $io.unlink;  # throw actual failures
                last unless $io.e;
                sleep .1;
            }
        }
        else {
            .throw without $io.unlink;      # throw actual failures
        }
    }

    method remove-artifacts(CompUnit::Repository::Staging:D: --> Nil) {
        my $io := self.prefix;
        really-unlink($io.child("version"));
        really-unlink(.IO) for Rakudo::Internals.DIR-RECURSE(
          $io.absolute, :file(*.ends-with(".lock"))
        );
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
        self-destruct self.prefix;
    }
}

# vim: expandtab shiftwidth=4
