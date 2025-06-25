Rakudo::Internals.REGISTER-DYNAMIC: '$*RAKUDO_MODULE_DEBUG', {
    PROCESS::<$RAKUDO_MODULE_DEBUG> := ?%*ENV<RAKUDO_MODULE_DEBUG>
      ?? -> *@str --> Nil {
            state $level = %*ENV<RAKUDO_MODULE_DEBUG>++;
            state $root  = $*CWD.Str;
            my $indent = (($level - 1) * 4) + 1;
            note sprintf "%2d%sRMD: %s",
              $level,
              " " x $indent,
              @str>>.indent(7 + $indent)
                .join("\n")
                .substr(7 + $indent)
                .subst($root, '.');
         }
      !! ?%*ENV<RAKUDO_PRECOMPILATION_PROGRESS>
        ?? -> $note --> Nil {
              state $level = %*ENV<RAKUDO_PRECOMPILATION_PROGRESS>++ - 1;
              state $module;
              my $message := $note.trim-leading;
              if $message.starts-with("Late loading '") {
                  $module = $message.substr(14, *-1);
              }
              elsif $message.starts-with("Precompiling ") {
                  note " " x $level ~ "Precompiling $module";
              }
           }
        !! False
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*EXECUTABLE', {
    PROCESS::<$EXECUTABLE> := IO::Path.new(:CWD(INIT nqp::cwd()),
      nqp::execname()
#?if jvm
      || $*VM.properties<perl6.prefix> ~ '/bin/perl6-j'
#?endif
#?if moar
      || ($*VM.config<prefix> ~ '/bin/'
        ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.exe' !! 'perl6-m'))
#?endif
#?if js
      // ($*VM.config<prefix> ~ '/bin/'
        ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-js.bat' !! 'perl6-js'))
#?endif
    );
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*EXECUTABLE-NAME', {
    PROCESS::<$EXECUTABLE-NAME> := $*EXECUTABLE.basename;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*PROGRAM-NAME', {
    PROCESS::<$PROGRAM-NAME> = nqp::getcomp('Raku').user-progname;
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
    if $*W -> $world {
        $world.suspend_recording_precompilation_dependencies;
        CompUnit::RepositoryRegistry.resolve-unknown-repos($repo);
        $world.resume_recording_precompilation_dependencies;
    }
    else {
        CompUnit::RepositoryRegistry.resolve-unknown-repos($repo);
    }
    PROCESS::<$REPO> # Cannot be $repo, as CU:RepositoryRegistry changes $*REPO
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

Rakudo::Internals.REGISTER-DYNAMIC: '$*USER',  {
    Rakudo::Internals.FETCH-USER-GROUP('$USER')
}
Rakudo::Internals.REGISTER-DYNAMIC: '$*GROUP', {
    Rakudo::Internals.FETCH-USER-GROUP('$GROUP')
}

# vim: expandtab shiftwidth=4
