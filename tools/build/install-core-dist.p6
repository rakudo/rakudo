use lib <lib>;
use CompUnit::Repository::Staging;
my %provides = 
    "Test"                          => "lib/Test.pm6",
    "NativeCall"                    => "lib/NativeCall.pm6",
    "NativeCall::Types"             => "lib/NativeCall/Types.pm6",
    "NativeCall::Compiler::GNU"     => "lib/NativeCall/Compiler/GNU.pm6",
    "NativeCall::Compiler::MSVC"    => "lib/NativeCall/Compiler/MSVC.pm6",
    "Pod::To::Text"                 => "lib/Pod/To/Text.pm6",
    "newline"                       => "lib/newline.pm6",
    "experimental"                  => "lib/experimental.pm6",
    "CompUnit::Repository::Staging" => "lib/CompUnit/Repository/Staging.pm6",
    "Telemetry"                     => "lib/Telemetry.pm6",
    "snapper"                       => "lib/snapper.pm6",
;

%provides<MoarVM::Profiler> = "lib/MoarVM/Profiler.pm6" if $*VM.name eq 'moar';

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
