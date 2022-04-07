#!/usr/bin/env raku

#| Remove *all* precomp files from the repository chain
unit sub MAIN();

sub unlink-recursively(IO::Path:D $io --> Nil) {
    .d ?? unlink-recursively($_) !! .unlink for $io.dir;
}

for $*REPO.repo-chain -> $repo {
    unlink-recursively(.prefix) with $repo.precomp-store;
}

# vim: expandtab shiftwidth=4
