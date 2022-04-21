class Distribution::Path does Distribution::Locally {
    has          %.meta is built(False);
    has IO::Path $.meta-file;

    method new(
      IO::Path:D  $prefix,
      IO::Path:D :$meta-file = $prefix.add('META6.json'),
    ) {
        self.bless(:$prefix, :$meta-file)
    }

    sub forward-slash(Str() $path) { $path.subst('\\', '/', :g) }

    submethod TWEAK(--> Nil) {
        die "No meta file located at $!meta-file.path()"
          unless $!meta-file.e;
        %!meta := Rakudo::Internals::JSON.from-json($!meta-file.slurp);
        %!meta<files> := my %files;

        # set up scripts in bin from file system (not in META)
        for Rakudo::Internals.DIR-RECURSE($!prefix.add('bin').absolute) {
            my $io  := .IO;
            my $script := forward-slash
              $io.is-relative ?? $io !! $io.relative($!prefix);
            %files{$script} := $script;
        }

        # Set up resources
        if %!meta<resources> -> @resources {
            my $resources-dir := $!prefix.add('resources');
            for @resources {
                if nqp::can($_,'chars') && .chars {
                    # set up path on filesystem, allowing for external libraries
                    my $path := (
                      .starts-with('libraries/')                    # 10 chars
                        ?? $resources-dir                           #  |
                              .add('libraries')                     #  v
                              .add($*VM.platform-library-name(.substr(10).IO))
                        !! $resources-dir.add($_)
                    ).relative($!prefix);

                    # set up the key by which the resource can be reached
                    my $io := .IO;
                    my $key := "resources/" ~ ($io.is-relative
                      ?? $_
                      !! $io.relative($!prefix)
                    );
                    %files{forward-slash($key)} := forward-slash($path);
                }
            }
        }
    }

    method meta(Distribution::Path:D:) { %!meta.item }
    method raku(--> Str:D) {
       self.^name ~ ".new($!prefix.raku(), meta-file => $!meta-file.raku())"
    }
}

# vim: expandtab shiftwidth=4
