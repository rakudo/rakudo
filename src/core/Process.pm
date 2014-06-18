{
    PROCESS::<$PID> := nqp::p6box_i(nqp::getpid());

    my $EXECUTABLE =
#?if parrot
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
#?endif
#?if jvm
        $*VM.properties<perl6.execname>
        // $*VM.properties<perl6.prefix> ~ '/bin/perl6-j';
#?endif
#?if moar
        nqp::execname()
        // ($*VM.config<prefix> ~ '/bin/' ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'));
#?endif
    $EXECUTABLE := $EXECUTABLE.path.absolute;
    PROCESS::<$EXECUTABLE>      := $EXECUTABLE;
    PROCESS::<$EXECUTABLE_NAME> := $EXECUTABLE.basename;

    my Mu $comp := nqp::getcomp('perl6');
    my $PROGRAM_NAME = $comp.user-progname();
    PROCESS::<$PROGRAM_NAME> = $PROGRAM_NAME;
    PROCESS::<$PROGRAM> = IO::Path.new($PROGRAM_NAME);

    PROCESS::<$TMPDIR> = IO::Spec.tmpdir().path;

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
