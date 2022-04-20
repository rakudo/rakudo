#!/usr/bin/env raku

#| Remove old precomp files from the repository chain
unit sub MAIN();

sub unlink-recursively(IO::Path:D $io, :$keep --> Nil) {
    .d
      ?? .basename ne $?COMPILATION-ID
        ?? unlink-recursively($_)
        !! Nil
      !! .unlink for $io.dir;
    $io.rmdir unless $keep;
}

for $*REPO.repo-chain -> $repo {
    unlink-recursively(.add("precomp"), :keep) with $repo.?prefix;
}

# vim: expandtab shiftwidth=4
