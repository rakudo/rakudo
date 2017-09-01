#!/usr/bin/env perl6
use v6.c;
use CompUnit::Repository::Staging;

# Distribution::Path ignores META.info files, but we can manually set it
sub find-meta-file($dir) {
    ('META6.json', 'META.info').map({$dir.child($_)}).first: {$_ ~~ :f}
}

multi sub MAIN(:from(:$dist-prefix) = '.', :to(:$repo-prefix)!, :for(:$repo-name)!) {
    my $meta-file = find-meta-file($dist-prefix.IO);
    my $dist      = Distribution::Path.new($dist-prefix.IO, :$meta-file);

    build(:dist-prefix($dist-prefix.IO));

    CompUnit::Repository::Staging.new(
        :prefix($repo-prefix),
        :next-repo(CompUnit::RepositoryRegistry.repository-for-name($repo-name)),
        :name($repo-name),
    ).install($dist);

    $_.unlink for <version repo.lock precomp/.lock>.map: {$repo-prefix.IO.child($_)};
}

multi sub MAIN(:from(:$dist-prefix) = '.', :to(:$repo-id) = 'site', Bool :$force) {
    my $meta-file = find-meta-file($dist-prefix.IO);
    my $dist      = Distribution::Path.new($dist-prefix.IO, :$meta-file);

    build(:dist-prefix($dist-prefix.IO));

    my $repo = first * ~~ CompUnit::Repository::Installable,
        CompUnit::RepositoryRegistry.repository-for-name($repo-id),
        CompUnit::RepositoryRegistry.repository-for-spec($repo-id)
            or die "Repository '$repo-id' is not an installable target";

    $repo.install($dist, :$force);
}

sub build(IO::Path :$dist-prefix) {
    my $meta-file = find-meta-file($dist-prefix);
    my $meta = Rakudo::Internals::JSON.from-json($meta-file.slurp);

    if ($meta<builder>) {
        my $builder = (require ::("Distribution::Builder::$meta<builder>")).new(:$meta);
        if $builder.can-build {
            $builder.build;
            exit;
        }
        else {
            note "Failed to build";
            exit 1;
        }
    }
}

# vim: ft=perl6
