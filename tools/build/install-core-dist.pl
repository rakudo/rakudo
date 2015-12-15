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

CompUnit::RepositoryRegistry.repository-for-name('perl').install(
    Distribution.new(
        name     => "CORE",
        auth     => "perl",
        ver      => $*PERL.version.Str,
        provides => %provides,
    ),
    %provides,
    :force,
);

note "installed!";

# vim: ft=perl6
