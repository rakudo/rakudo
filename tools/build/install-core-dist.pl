my %provides = 
    "Test"                       => "lib/Test.pm6",
    "NativeCall"                 => "lib/NativeCall.pm6",
    "NativeCall::Types"          => "lib/NativeCall/Types.pm6",
    "NativeCall::Compiler::GNU"  => "lib/NativeCall/Compiler/GNU.pm6",
    "NativeCall::Compiler::MSVC" => "lib/NativeCall/Compiler/MSVC.pm6",
    "Pod::To::Text"              => "lib/Pod/To/Text.pm6",
    "newline"                    => "lib/newline.pm6",
    "experimental"               => "lib/experimental.pm6",
;

PROCESS::<$REPO> := CompUnit::RepositoryRegistry.repository-for-spec("inst#@*ARGS[0]");
my $dist = Distribution::Hash.new(%(
    name     => "CORE",
    auth     => "perl",
    ver      => $*PERL.version.Str,
    provides => %provides,
), prefix => $*CWD);
$*REPO.install($dist, :force);

note "installed!";

# vim: ft=perl6
