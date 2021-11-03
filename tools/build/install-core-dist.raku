use v6.d;

use lib <tools/build>;
use Staging;
use META;

my $prefix := @*ARGS[0];
my $REPO := PROCESS::<$REPO> := CompUnit::Repository::Staging.new(
    :$prefix
    :next-repo(
        # Make CompUnit::Repository::Staging available to precomp processes
        CompUnit::Repository::Installation.new(
            :$prefix
            :next-repo(CompUnit::RepositoryRegistry.repository-for-name('core')),
        )
    ),
    :name('core'),
);
$REPO.install(
    Distribution::Hash.new(
        %Rakudo::CORE::META,
        prefix => $*CWD,
    ),
    :force,
);

# Precompile CompUnit::Repository::Staging again to give it a source path relative to perl#
my $core-dist := $REPO.resolve(
    CompUnit::DependencySpecification.new(
      :short-name<CompUnit::Repository::Staging>)
).distribution;

my $source-id :=
  $core-dist.meta<provides><CompUnit::Repository::Staging>.values.head<file>;
my $source      := $REPO.prefix.child('sources').child($source-id);
my $source-file := $source.relative($REPO.prefix);


$REPO.precomp-repository.precompile(
        $source,
        CompUnit::PrecompilationId.new($source-id),
        :source-name("core#$source-file (CompUnit::Repository::Staging)"),
        :force,
);

note "    Installed {%Rakudo::CORE::META<provides>.elems} core modules in {now - INIT now} seconds!";

# vim: expandtab sw=4
