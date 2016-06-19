#!/usr/bin/env perl6
use v6.c;

role Distribution::Directory {
    has IO::Path $.path;
    has %!meta;

    method meta() {
        %!meta ||= Rakudo::Internals::JSON.from-json: slurp ('META6.json', 'META.info').map({$.path.child($_)}).first: {$_ ~~ :f};
    }

    method sources() {
        my %sources = self.meta<provides>;
        $_ = $.path.child($_) for %sources.values;
        %sources
    }

    method scripts() {
        my %scripts;
        my $bin-dir = $.path.child('bin');
        if $bin-dir ~~ :d {
            for $bin-dir.dir -> $bin {
                my $basename = $bin.basename;
                next if $basename.substr(0, 1) eq '.';
                next if !$*DISTRO.is-win and $basename ~~ /\.bat$/;
                %scripts{$basename} = ~$bin;
            }
        }
        %scripts
    }

    method resources {
        my $resources-dir = $.path.child('resources');
        %( (self.meta<resources> // []).map({
            $_ => $_ ~~ m/^libraries\/(.*)/
                ?? ~$resources-dir.child('libraries').child($*VM.platform-library-name($0.Str.IO))
                !! ~$resources-dir.child($_)
        }) );
    }
}

class CompUnit::Repository::Staging is CompUnit::Repository::Installation {
    has Str $.name;

    submethod BUILD(Str :$!name --> Nil) { }

    method name(--> Str) {
        $!name
    }
    method source-file(Str $name --> IO::Path) {
        my $file = self.prefix.child($name);
        $file.e ?? $file !! self.next-repo.source-file($name)
    }
}

multi sub MAIN($from is copy = '.', :$to!, :$for!) {
    $from = $from.IO;
    my $dist-dir = Distribution::Directory.new(path => $from);

    CompUnit::Repository::Staging.new(
        :prefix($to),
        :next-repo(CompUnit::RepositoryRegistry.repository-for-name($for)),
        :name($for),
    ).install(
        Distribution.new(|$dist-dir.meta),
        $dist-dir.sources,
        $dist-dir.scripts,
        $dist-dir.resources,
    );

    $_.unlink for <version repo.lock precomp/.lock>.map: {"$to/$_".IO};
}

multi sub MAIN($from is copy = '.', :$to = 'site') {
    $from = $from.IO;
    my $dist-dir = Distribution::Directory.new(path => $from);

    my $repo = $to ~~ /^\w+$/
        ?? CompUnit::RepositoryRegistry.repository-for-name($to)
        !! CompUnit::RepositoryRegistry.repository-for-spec($to);
    $repo.install(
        Distribution.new(|$dist-dir.meta),
        $dist-dir.sources,
        $dist-dir.scripts,
        $dist-dir.resources,
    );
}

# vim: ft=perl6
