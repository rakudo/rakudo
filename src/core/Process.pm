multi sub INITIALIZE_DYNAMIC('$*PID') {
    PROCESS::<$PID> := nqp::p6box_i(nqp::getpid());
}

multi sub INITIALIZE_DYNAMIC('$*EXECUTABLE') {
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

multi sub INITIALIZE_DYNAMIC('$*EXECUTABLE-NAME') {
    PROCESS::<$EXECUTABLE-NAME> := $*EXECUTABLE.basename;
}

multi sub INITIALIZE_DYNAMIC('$*PROGRAM-NAME') {
    PROCESS::<$PROGRAM-NAME> := nqp::getcomp('perl6').user-progname;
}

multi sub INITIALIZE_DYNAMIC('$*PROGRAM') {
    PROCESS::<$PROGRAM> := IO::Path.new($*PROGRAM-NAME);
}

multi sub INITIALIZE_DYNAMIC('$*TMPDIR') {
    PROCESS::<$TMPDIR> := $*SPEC.tmpdir;
}

multi sub INITIALIZE_DYNAMIC('$*HOME') {
    my $HOME;

    if %*ENV<HOME> -> $home {
        $HOME = $home;
    }
    elsif $*DISTRO.is-win {
        $HOME = %*ENV<HOMEDRIVE> ~ %*ENV<HOMEPATH>;
    }
    PROCESS::<$HOME> = $HOME ?? IO::Path.new($HOME) !! Nil;
}

{
    class IdName {
        has Int $!id;
        has Str $!name;

        submethod BUILD (:$!id, :$!name) { }

        method Numeric { $!id }
        method Str     { $!name }
        method gist    { "$!name ($!id)" }
    }

    class IdFetch {
        has Str $!name;

        submethod BUILD (:$!name) { PROCESS::{$!name} := self }

        sub fetch {
            once if !$*DISTRO.is-win && try { qx/id/ } -> $id {
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

        method Numeric { return Nil unless fetch(); +PROCESS::{$!name} }
        method Str     { return Nil unless fetch(); ~PROCESS::{$!name} }
        method gist    {
            return Nil unless fetch();
            PROCESS::{$!name} ~ ' (' ~ +PROCESS::{$!name} ~ ')';
        }
    }

    IdFetch.new( :name<$USER> );
    IdFetch.new( :name<$GROUP> );
}

# Deprecations
multi sub INITIALIZE_DYNAMIC('$*EXECUTABLE_NAME') {
    $*EXECUTABLE-NAME; # prime it
    PROCESS::<$EXECUTABLE_NAME> := PROCESS::<$EXECUTABLE-NAME>;
}
multi sub INITIALIZE_DYNAMIC('$*PROGRAM_NAME') {
    $*PROGRAM-NAME;  # prime it
    PROCESS::<$PROGRAM_NAME> := PROCESS::<$PROGRAM-NAME>;
}

# vim: ft=perl6 expandtab sw=4
