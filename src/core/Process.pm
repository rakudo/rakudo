multi sub INITIALIZE_DYNAMIC('$*RAKUDO_MODULE_DEBUG') {
    PROCESS::<$RAKUDO_MODULE_DEBUG> := +?%*ENV<RAKUDO_MODULE_DEBUG>;
}

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
