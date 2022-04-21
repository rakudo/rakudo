#!/usr/bin/env raku

my str $compilation-id = Compiler.id;

#| Remove old precomp files from the repository chain
unit sub MAIN();

sub unlink-recursively(IO::Path:D $io, :$keep --> Int:D) {
    my int $bytes;
    for $io.dir {
        if .d {
            $bytes += unlink-recursively($_)
              if .basename ne $compilation-id;
        }
        else {
            $bytes += .s;
            .unlink;
        }
    }

    $io.rmdir unless $keep;
    $bytes
}

my int $repositories;
my int $total;
my @repos := $*REPO.repo-chain;
for $*REPO.repo-chain.map({ $_ with .?prefix }) {
    $total += unlink-recursively(.add("precomp"), :keep);
    ++$repositories;
}
say "Freed up $total.flip.comb(3).join(",").flip() bytes from $repositories repositories.";

# vim: expandtab shiftwidth=4
