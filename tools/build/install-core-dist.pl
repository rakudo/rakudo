use lib "inst#repo";
my $v = run("git", "describe", :out).out.lines[0];
$v //= $*PERL6.version;

my %provides = 
    "Test"                       => "lib/Test.pm",
    "NativeCall"                 => "lib/NativeCall.pm",
    "NativeCall::Types"          => "lib/NativeCall/Types.pm",
    "NativeCall::Compiler::GNU"  => "lib/NativeCall/Compiler/GNU.pm",
    "NativeCall::Compiler::MSVC" => "lib/NativeCall/Compiler/MSVC.pm",
    "Pod::To::Text"              => "lib/Pod/To/Text.pm",
;

$*REPO.repo-chain[* - 1].install(
    dist => class :: {
        method metainfo() {
            {
                name => "CORE",
                auth => "perl",
                ver => $v.Str,
                provides => %provides,
            }
        }
    }.new,
    |%provides.values,
);

note "installed!";

# vim: ft=perl6
