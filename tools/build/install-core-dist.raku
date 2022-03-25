my %provides = 
    "Test"                          => "lib/Test.rakumod",
    "NativeCall"                    => "lib/NativeCall.rakumod",
    "NativeCall::Types"             => "lib/NativeCall/Types.rakumod",
    "NativeCall::Compiler::GNU"     => "lib/NativeCall/Compiler/GNU.rakumod",
    "NativeCall::Compiler::MSVC"    => "lib/NativeCall/Compiler/MSVC.rakumod",
    "Pod::To::Text"                 => "lib/Pod/To/Text.rakumod",
    "newline"                       => "lib/newline.rakumod",
    "experimental"                  => "lib/experimental.rakumod",
    "Telemetry"                     => "lib/Telemetry.rakumod",
    "snapper"                       => "lib/snapper.rakumod",
    "safe-snapper"                  => "lib/safe-snapper.rakumod",
    "BUILDPLAN"                     => "lib/BUILDPLAN.rakumod",
;

%provides<NativeCall::Dispatcher> = "lib/NativeCall/Dispatcher.rakumod"
    if $*RAKU.compiler.?supports-op('dispatch_v');

if Compiler.backend eq 'moar' {
    %provides<MoarVM::Profiler> = "lib/MoarVM/Profiler.rakumod";
    %provides<MoarVM::Spesh>    = "lib/MoarVM/Spesh.rakumod";
    %provides<MoarVM::SL>       = "lib/MoarVM/SL.rakumod";
    %provides<SL>               = "lib/SL.rakumod";
    %provides<MoarVM::SIL>      = "lib/MoarVM/SIL.rakumod";
    %provides<SIL>              = "lib/SIL.rakumod";
}

my $core-repo = CompUnit::RepositoryRegistry.repository-for-name('core');
my $core-repo-prefix = $core-repo.prefix;
my $staging-prefix = $*TMPDIR.add('staging');

my $REPO := PROCESS::<$REPO> := CompUnit::Repository::Staging.new(
    :prefix($staging-prefix),
    :next-repo($core-repo), # Make CompUnit::Repository::Staging available to precomp processes
    :name('core'),
);

$REPO.self-destruct();

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

$REPO.remove-artifacts();
$REPO.deploy();
$REPO.self-destruct();

note "    Installed {%provides.elems} core modules in {now - INIT now} seconds!";

# vim: expandtab sw=4
