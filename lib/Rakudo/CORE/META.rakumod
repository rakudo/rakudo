unit class Rakudo::CORE;

our %META = BEGIN { 
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
      "safe-snapper"                  => "lib/safe-snapper.rakumod",
      "BUILDPLAN"                     => "lib/BUILDPLAN.rakumod",
      "Rakudo::CORE::META"            => "lib/Rakudo/CORE/META.rakumod",
    ;

    if Compiler.backend eq 'moar' {
        %provides<MoarVM::Profiler> = "lib/MoarVM/Profiler.rakumod";
        %provides<MoarVM::Spesh>    = "lib/MoarVM/Spesh.rakumod";
        %provides<MoarVM::SL>       = "lib/MoarVM/SL.rakumod";
        %provides<SL>               = "lib/SL.rakumod";
        %provides<MoarVM::SIL>      = "lib/MoarVM/SIL.rakumod";
        %provides<SIL>              = "lib/SIL.rakumod";
    }

    %(
      name     => 'CORE',
      auth     => 'perl',
      ver      => $*RAKU.version.Str,
      provides => %provides,
    )
}

# vim: expandtab sw=4
