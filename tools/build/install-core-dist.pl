my $v = run("git", "describe", :out).out.lines[0];
$v //= $*PERL.version;

my %provides = 
    "Test"                       => "lib/Test.pm",
    "NativeCall"                 => "lib/NativeCall.pm",
    "NativeCall::Types"          => "lib/NativeCall/Types.pm",
    "NativeCall::Compiler::GNU"  => "lib/NativeCall/Compiler/GNU.pm",
    "NativeCall::Compiler::MSVC" => "lib/NativeCall/Compiler/MSVC.pm",
    "Pod::To::Text"              => "lib/Pod/To/Text.pm",
;

$*REPO.repo-chain[* - 1].install(
    Distribution.new(
        name     => "CORE",
        auth     => "perl",
        ver      => $v.Str,
        provides => %provides,
    ),
    %provides,
    :force,
);

note "installed!";

# vim: ft=perl6
