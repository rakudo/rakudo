role CompUnit::PrecompilationUnit {
    method id(--> CompUnit::PrecompilationId:D) { ... }
    method path(--> IO::Path:D) { ... }
    method modified(--> Instant:D) { ... }
    method dependencies(--> Array[CompUnit::PrecompilationDependency]) { ... }
    method bytecode(--> Buf:D) { ... }
    method checksum(--> Str:D) { ... }
    method source-checksum(--> Str:D) { ... }
    method bytecode-handle(--> IO::Handle:D) { ... }
    method close(--> Nil) { ... }

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
                "$.path\nspec: $dependency.spec()\nsource: $srcIO\n"
                ~ "source-checksum: $.source-checksum\ncurrent-source-checksum: $current-source-checksum"
            ) if $RMD;

            return False if $.source-checksum ne $current-source-checksum;
        }

        $RMD("dependency checksum $dependency.checksum() unit: $.checksum()")
          if $RMD;

        $.checksum eq $dependency.checksum
    }
}

# vim: expandtab shiftwidth=4
