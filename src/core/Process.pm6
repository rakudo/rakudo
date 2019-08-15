Rakudo::Internals.REGISTER-DYNAMIC: '$*RAKUDO_MODULE_DEBUG', {
    PROCESS::<$RAKUDO_MODULE_DEBUG> := ?%*ENV<RAKUDO_MODULE_DEBUG>
      ?? -> *@str --> Nil {
            state $level = %*ENV<RAKUDO_MODULE_DEBUG>++;
            my $indent = (($level - 1) * 4) + 1;
            my $str = @str>>.indent(7 + $indent).join("\n").substr(7 + $indent);
            note sprintf "%2d%sRMD: $str", $level, " " x $indent;
         }
      !! False
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*EXECUTABLE', {
    PROCESS::<$EXECUTABLE> := IO::Path.new(:CWD(INIT nqp::cwd()),
#?if jvm
      $*VM.properties<perl6.execname>
      // $*VM.properties<perl6.prefix> ~ '/bin/perl6-j'
#?endif
#?if moar
      nqp::execname()
      || ($*VM.config<prefix> ~ '/bin/'
        ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.exe' !! 'perl6-m'))
#?endif
#?if js
      nqp::execname()
      // ($*VM.config<prefix> ~ '/bin/'
        ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-js.bat' !! 'perl6-js'))
#?endif
    );
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*EXECUTABLE-NAME', {
    PROCESS::<$EXECUTABLE-NAME> := $*EXECUTABLE.basename;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*PROGRAM-NAME', {
    PROCESS::<$PROGRAM-NAME> := nqp::getcomp('perl6').user-progname;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*PROGRAM', {
    PROCESS::<$PROGRAM> := IO::Path.new(:CWD(INIT nqp::cwd()), $*PROGRAM-NAME);
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TMPDIR', {
    PROCESS::<$TMPDIR> = $*SPEC.tmpdir;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TOLERANCE', {
    PROCESS::<$TOLERANCE> := item 1e-15;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*REPO', {
    my $repo := PROCESS::<$REPO> := CompUnit::RepositoryRegistry.setup-repositories;
    my $world := $*W;
    $world.suspend_recording_precompilation_dependencies if $world;
    CompUnit::RepositoryRegistry.resolve-unknown-repos($repo);
    $world.resume_recording_precompilation_dependencies if $world;
    PROCESS::<$REPO>
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*HOME', {
    my $HOME is default(Nil);

    if %*ENV<HOME> -> $home {
        $HOME = $home;
    }
    elsif Rakudo::Internals.IS-WIN {
        my $env := %*ENV;
        $env<HOMEDRIVE> && $env<HOMEPATH> && ($HOME
          = nqp::concat($env<HOMEDRIVE>, $env<HOMEPATH>));
    }

    $HOME = IO::Path.new($HOME) if $HOME;
    PROCESS::<$HOME> := $HOME # bind container so Nil default is kept
}

{
    sub fetch($what) {
        once if !Rakudo::Internals.IS-WIN && try { qx/id/ } -> $id {
            if $id ~~ m/^
              [ uid "=" $<uid>=(\d+) ]
              [ "(" $<user>=(<-[ ) ]>+) ")" ]
              \s+
              [ gid "=" $<gid>=(\d+) ]
              [ "(" $<group>=(<-[ ) ]>+) ")" ]
            / {
                PROCESS::<$USER>  := IntStr.new(+$<uid>,~$<user>);
                PROCESS::<$GROUP> := IntStr.new(+$<gid>,~$<group>);
            }

            # alas, no support yet
            else {
                PROCESS::<$USER>  := Nil;
                PROCESS::<$GROUP> := Nil;
            }
        }
        PROCESS::{$what}
    }

    Rakudo::Internals.REGISTER-DYNAMIC: '$*USER',  { fetch('$USER') };
    Rakudo::Internals.REGISTER-DYNAMIC: '$*GROUP', { fetch('$GROUP') };
}

# vim: ft=perl6 expandtab sw=4
