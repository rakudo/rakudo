role CompUnit::PrecompilationUnit {
    method id(--> CompUnit::PrecompilationId:D) { ... }
    method path(--> IO::Path:D) { ... }
    method modified(--> Instant:D) { ... }
    method dependencies(--> Array[CompUnit::PrecompilationDependency]) { ... }
    method bytecode(--> Buf:D) { ... }
    method checksum(--> Str:D) { ... }
    method source-checksum(--> Str:D) { ... }
    method dist-checksum(--> Str:D) { ... }
    method bytecode-handle(--> IO::Handle:D) { ... }
    method close(--> Nil) { ... }

    my class CSDist does Distribution::Checksum {
        has %.meta;
        method !SET-SELF(%!meta) { self }
        method new(%meta) {
            nqp::create(self)!SET-SELF(%meta)
        }
        method content {}
    }

    method is-up-to-date(
      CompUnit::PrecompilationDependency $dependency,
      Bool :$check-source
    --> Bool:D) {
        my $RMD := $*RAKUDO_MODULE_DEBUG;

        # a repo changed, so maybe it's a change in our source file
        if $check-source {
            my $srcIO :=
              CompUnit::RepositoryRegistry.file-for-spec($dependency.src)
              // $dependency.src.IO;
            return False unless $srcIO.e;

            my $current-source-checksum := $srcIO.CHECKSUM;

            $RMD(
                "$.path source checkumming:\nspec: $dependency.spec()\nsource: $srcIO\n"
                ~ "source-checksum: $.source-checksum\ncurrent-source-checksum: $current-source-checksum"
            ) if $RMD;

            return False if $.source-checksum ne $current-source-checksum;
        }

        # There is no sense in distribution checksumming for auto-generated distributions, like those produced for 
        # -I<dir> where <dir> doesn't contain META6.json.
        if $dependency.dist-src {
          my $current-dist-checksum = 
              $dependency.dist-src 
                  ?? CSDist.new(
                      Rakudo::Internals::JSON.from-json(
                        (CompUnit::RepositoryRegistry.file-for-spec($dependency.dist-src)
                          // $dependency.dist-src.IO).slurp)).checksum
                  !! '';

          $RMD(
              "$.path dist checksumming:\n"
              ~ "  dependency spec: $dependency.spec()\n"
              ~ "  dist-checksum, old: $.dist-checksum, current (" ~ $dependency.dist-src ~ "): $current-dist-checksum"
          ) if $RMD;

          return False if self.dist-checksum ne $current-dist-checksum;
        }

        $RMD("dependency checksum $dependency.checksum() unit: $.checksum()")
          if $RMD;

        $.checksum eq $dependency.checksum
    }
}

# vim: expandtab shiftwidth=4
