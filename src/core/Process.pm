Rakudo::Internals.REGISTER-DYNAMIC: '$*RAKUDO_MODULE_DEBUG', {
    PROCESS::<$RAKUDO_MODULE_DEBUG> := ?%*ENV<RAKUDO_MODULE_DEBUG>
      ?? -> *@str --> Nil {
            state Num $last = Rakudo::Internals.INITTIME;
            my num $now = nqp::time_n;
            my $str = @str>>.indent(16).join("\n").substr(16);
            note sprintf "%4d %5d RMD: $str",
              1000 * ($now - $last), nqp::getpid();
            $last = $now;
         }
      !! False
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*PID', {
    PROCESS::<$PID> := nqp::p6box_i(nqp::getpid());
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*EXECUTABLE', {
    PROCESS::<$EXECUTABLE> := IO::Path.new-from-absolute-path(
#?if jvm
      $*VM.properties<perl6.execname>
      // $*VM.properties<perl6.prefix> ~ '/bin/perl6-j'
#?endif
#?if moar
      nqp::execname()
      // ($*VM.config<prefix> ~ '/bin/'
        ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'))
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
    PROCESS::<$PROGRAM> := IO::Path.new($*PROGRAM-NAME);
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TMPDIR', {
    PROCESS::<$TMPDIR> := $*SPEC.tmpdir;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TOLERANCE', {
    PROCESS::<$TOLERANCE> := item 1e-15;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*REPO', {
    PROCESS::<$REPO> := CompUnit::RepositoryRegistry.setup-repositories;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*HOME', {
    my $HOME;

    if %*ENV<HOME> -> $home {
        $HOME = $home;
    }
    elsif Rakudo::Internals.IS-WIN {
        $HOME = %*ENV<HOMEDRIVE> ~ %*ENV<HOMEPATH>;
    }
    PROCESS::<$HOME> = $HOME ?? IO::Path.new($HOME) !! Nil;
}

{
    class IdName {
        has Int $!id;
        has Str $!name;

        submethod BUILD(:$!id, :$!name --> Nil) { }

        method Numeric { $!id }
        method Str     { $!name }
        method gist    { "$!name ($!id)" }
    }

    class IdFetch {
        has Str $!name;

        submethod BUILD(:$!name --> Nil) { PROCESS::{$!name} := self }

        sub fetch {
            once if !Rakudo::Internals.IS-WIN && try { qx/id/ } -> $id {
                if $id ~~ m/^
                  [ uid "=" $<uid>=(\d+) ]
                  [ "(" $<user>=(<-[ ) ]>+) ")" ]
                  \s+
                  [ gid "=" $<gid>=(\d+) ]
                  [ "(" $<group>=(<-[ ) ]>+) ")" ]
                / {
                    PROCESS::<$USER> :=
                      IdName.new( :id(+$<uid>), :name(~$<user>) );
                    PROCESS::<$GROUP> :=
                      IdName.new( :id(+$<gid>), :name(~$<group>) );
                }

                # alas, no support yet
                else {
                    PROCESS::<$USER>  := Nil;
                    PROCESS::<$GROUP> := Nil;
                }
            }
        }

        multi method Numeric(IdFetch:D:) {
            fetch() ?? +PROCESS::{$!name} !! Nil;
        }
        multi method Str(IdFetch:D:) {
            fetch() ?? ~PROCESS::{$!name} !! Nil;
        }
        multi method gist(IdFetch:D:) {
            fetch() ?? "{PROCESS::{$!name}} ({+PROCESS::{$!name}})" !! Nil;
        }
    }

    IdFetch.new( :name<$USER> );
    IdFetch.new( :name<$GROUP> );
}

# vim: ft=perl6 expandtab sw=4
