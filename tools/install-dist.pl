#!/usr/bin/env perl6
use v6.c;
use CompUnit::Repository::Staging;

sub find-meta-file($dir) {
    ('META6.json', 'META.info').map({$dir.child($_)}).first: {$_ ~~ :f}
}

multi sub MAIN($from is copy = '.', :$to!, :$for!) {
    $from = $from.IO;
    my $dist-dir = Distribution::Path.new($from, :file(find-meta-file($from)));

    CompUnit::Repository::Staging.new(
        :prefix($to),
        :next-repo(CompUnit::RepositoryRegistry.repository-for-name($for)),
        :name($for),
    ).install(
        Distribution::Hash.new($dist-dir.meta, :prefix($from)),
    );

    $_.unlink for <version repo.lock precomp/.lock>.map: {"$to/$_".IO};
}

multi sub MAIN($from is copy = '.', :$to = 'site', Bool :$force) {
    $from = $from.IO;
    my $dist-dir = Distribution::Path.new($from, :file(find-meta-file($from)));

    my $repo = $to ~~ /^\w+$/
        ?? CompUnit::RepositoryRegistry.repository-for-name($to)
        !! CompUnit::RepositoryRegistry.repository-for-spec($to);
    $repo.install(
        Distribution::Hash.new($dist-dir.meta, :prefix($from)),
        :$force,
    );
}

# vim: ft=perl6
