multi sub INITIALIZE_DYNAMIC('$*PID') {
    PROCESS::<$PID> := nqp::p6box_i(nqp::getpid());
}

multi sub INITIALIZE_DYNAMIC('$*EXECUTABLE') {
    my $EXECUTABLE =
#?if jvm
        $*VM.properties<perl6.execname>
        // $*VM.properties<perl6.prefix> ~ '/bin/perl6-j';
#?endif
#?if moar
        nqp::execname()
        // ($*VM.config<prefix> ~ '/bin/' ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'));
#?endif
    $EXECUTABLE := IO::Path.new-from-absolute-path($EXECUTABLE);
    PROCESS::<$EXECUTABLE_NAME> := $EXECUTABLE.basename;
    PROCESS::<$EXECUTABLE>      := $EXECUTABLE;
}

multi sub INITIALIZE_DYNAMIC('$*EXECUTABLE_NAME') {
    PROCESS::<$EXECUTABLE_NAME> := $*EXECUTABLE.basename;
}

multi sub INITIALIZE_DYNAMIC('$*PROGRAM_NAME') {
    my Mu $comp := nqp::getcomp('perl6');
    my $PROGRAM_NAME = $comp.user-progname();
    PROCESS::<$PROGRAM>      := IO::Path.new($PROGRAM_NAME);
    PROCESS::<$PROGRAM_NAME> := $PROGRAM_NAME;
}

multi sub INITIALIZE_DYNAMIC('$*PROGRAM') {
    PROCESS::<$PROGRAM> := IO::Path.new($*PROGRAM_NAME);
}

multi sub INITIALIZE_DYNAMIC('$*TMPDIR') {
    PROCESS::<$TMPDIR> = $*SPEC.tmpdir;
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

# vim: ft=perl6 expandtab sw=4
