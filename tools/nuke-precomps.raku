#!/usr/bin/env raku

#| Remove *all* precomp files from the repository chain
unit sub MAIN();

sub unlink-recursively(IO::Path:D $io, :$keep --> Nil) {
    .d ?? unlink-recursively($_) !! .unlink for $io.dir;
    $io.rmdir unless $keep;
}

for $*REPO.repo-chain -> $repo {
    unlink-recursively(.add("precomp"), :keep) with $repo.?prefix;
}

# vim: expandtab shiftwidth=4
