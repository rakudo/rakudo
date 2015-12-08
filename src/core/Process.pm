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

multi sub INITIALIZE_DYNAMIC('$*TOLERANCE') {
    PROCESS::<$TOLERANCE> := item 1e-15;
}

multi sub INITIALIZE_DYNAMIC('$*REPO') {
    my @INC;
    my %CUSTOM_LIB;
    my %ENV := %*ENV; # only look up environment once

    # starting up for creating precomp
    if %ENV<RAKUDO_PRECOMP_WITH> -> \specs {
        @INC = specs.split(','); # assume well formed strings
    }

    # normal start up
    else {
        my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
        if nqp::defined($I) {
            if nqp::islist($I) {
                my Mu $iter := nqp::iterator($I);
                while $iter {
                    @INC.append: PARSE-INCLUDE-SPECS(nqp::shift($iter));
                }
           }
            else {
                @INC.append: PARSE-INCLUDE-SPECS(nqp::p6box_s($I));
            }
        }

        if %ENV<RAKUDOLIB> -> $rakudolib {
            @INC.append: PARSE-INCLUDE-SPECS($rakudolib);
        }
        if %ENV<PERL6LIB> -> $perl6lib {
            @INC.append: PARSE-INCLUDE-SPECS($perl6lib);
        }

#?if jvm
        for nqp::hllize(nqp::jvmclasspaths()) -> $path {
            @INC.append: PARSE-INCLUDE-SPECS($path);
        }
#?endif

        my $prefix := nqp::p6box_s(
          nqp::concat(nqp::atkey(nqp::backendconfig,'prefix'),'/share/perl6')
        );

        my $abspath := "$prefix/share/libraries.json";
        if IO::Path.new-from-absolute-path($abspath).e {
#            my $config = from-json( slurp $abspath );
#
#            for $config.list -> @group {
#                for @group>>.kv -> $class, $props {
#                    for $props.list -> $prop {
#                        if nqp::istype($prop,Associative) {
#                            for $prop.value.flat -> $path {
#                                @INC.push: PARSE-INCLUDE-SPECS($path);
#                                %CUSTOM_LIB{$prop.key} = $path;
#                            }
#                        }
#                        else {
#                            for $prop.flat -> $path {
#                                @INC.push: PARSE-INCLUDE-SPECS($path);
#                            }
#                        }
#                    }
#                }
#            }
        }
        # There is no config file, so pick sane defaults.
        else {
            # XXX Various issues with this stuff on JVM
            my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
            try {
                if %ENV<HOME>
                  // (%ENV<HOMEDRIVE> // '') ~ (%ENV<HOMEPATH> // '') -> $home {
                    my $ver := nqp::p6box_s(nqp::atkey($compiler, 'version'));
                    my $path := "$home/.perl6/$ver";
                    @INC.append: (%CUSTOM_LIB<home> = "inst#$path");
                }
            }
            @INC.append:
              (%CUSTOM_LIB<site>   = "inst#$prefix/site"),
              (%CUSTOM_LIB<vendor> = "inst#$prefix/vendor"),
              (%CUSTOM_LIB<perl>   = "inst#$prefix");
        }
    }

    PROCESS::<%CUSTOM_LIB> := %CUSTOM_LIB;

    my CompUnit::Repository $next-repo;
    my %repos;
    my $SPEC := $*SPEC;
    my &canon = -> $repo {
        my @parts = $repo.split('#');
        join '#', @parts[0], $SPEC.canonpath(@parts[1]);
    };
    %repos{$_} = $next-repo := CompUnit::RepositoryRegistry.new($_, :$next-repo) for @INC>>.&canon.unique.reverse;

    $_ = %repos{$_.&canon} for %CUSTOM_LIB.values;
    PROCESS::<$REPO> := $next-repo;
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
