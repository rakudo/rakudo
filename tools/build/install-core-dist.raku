use v6.d;

use lib <lib>;
use CompUnit::Repository::Staging;

my %provides = 
    "Test"                          => "lib/Test.rakumod",
    "NativeCall"                    => "lib/NativeCall.rakumod",
    "NativeCall::Types"             => "lib/NativeCall/Types.rakumod",
    "NativeCall::Compiler::GNU"     => "lib/NativeCall/Compiler/GNU.rakumod",
    "NativeCall::Compiler::MSVC"    => "lib/NativeCall/Compiler/MSVC.rakumod",
    "Pod::To::Text"                 => "lib/Pod/To/Text.rakumod",
    "newline"                       => "lib/newline.rakumod",
    "experimental"                  => "lib/experimental.rakumod",
    "CompUnit::Repository::Staging" => "lib/CompUnit/Repository/Staging.rakumod",
    "Telemetry"                     => "lib/Telemetry.rakumod",
    "snapper"                       => "lib/snapper.rakumod",
    "BUILDPLAN"                     => "lib/BUILDPLAN.rakumod",
;

if Compiler.backend eq 'moar' {
    %provides<MoarVM::Profiler> = "lib/MoarVM/Profiler.rakumod";
    %provides<MoarVM::Spesh>    = "lib/MoarVM/Spesh.rakumod";
    %provides<MoarVM::SL>       = "lib/MoarVM/SL.rakumod";
    %provides<SL>               = "lib/SL.rakumod";
    %provides<MoarVM::SIL>      = "lib/MoarVM/SIL.rakumod";
    %provides<SIL>              = "lib/SIL.rakumod";
}

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
        {
            name     => 'CORE',
            auth     => 'perl',
            ver      => $*RAKU.version.Str,
            provides => %provides,
        },
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

note "    Installed {%provides.elems} core modules in {now - INIT now} seconds!";

# vim: expandtab sw=4
