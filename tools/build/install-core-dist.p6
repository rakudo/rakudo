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
;

%provides<MoarVM::Profiler> = "lib/MoarVM/Profiler.rakumod"
  if $*VM.name eq 'moar';

PROCESS::<$REPO> := CompUnit::Repository::Staging.new(
    :prefix(@*ARGS[0]),
    :next-repo(
        # Make CompUnit::Repository::Staging available to precomp processes
        CompUnit::Repository::Installation.new(
            :prefix(@*ARGS[0]),
            :next-repo(CompUnit::RepositoryRegistry.repository-for-name('core')),
        )
    ),
    :name('core'),
);
$*REPO.install(
    Distribution::Hash.new(
        {
            name     => 'CORE',
            auth     => 'perl',
            ver      => $*PERL.version.Str,
            provides => %provides,
        },
        prefix => $*CWD,
    ),
    :force,
);

# Precompile CompUnit::Repository::Staging again to give it a source path relative to perl#
my $core-dist = $*REPO.resolve(
    CompUnit::DependencySpecification.new(:short-name<CompUnit::Repository::Staging>)
).distribution;
my $source-id = $core-dist.meta<provides><CompUnit::Repository::Staging>.values[0]<file>;
my $source = $*REPO.prefix.child('sources').child($source-id);
my $source-file = $source.relative($*REPO.prefix);
$*REPO.precomp-repository.precompile(
        $source,
        CompUnit::PrecompilationId.new($source-id),
        :source-name("core#$source-file (CompUnit::Repository::Staging)"),
        :force,
    );

#note "installed!";

# vim: ft=perl6
