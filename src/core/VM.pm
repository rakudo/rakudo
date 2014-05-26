class VM does Systemic {
    has $.config;
#?if jvm
    has $.properties;
#?endif
    has $.precomp-ext;
    has $.precomp-target;

    submethod BUILD (
      :$!name,
      :$!config,
#?if jvm
      :$!properties,
#?endif
    ) {
        $!auth    = "unknown";
        $!version = Version.new($!config<version> // "unknown");
        $!precomp-ext =
#?if parrot
          "pir"
#?endif
#?if jvm
          "jar"
#?endif
#?if moar
          "moarvm"
#?endif
# add new backends here please
        ;
        $!precomp-target =
#?if parrot
          "pir"
#?endif
#?if jvm
          "jar"
#?endif
#?if moar
          "mbc"
#?endif
# add new backends here please
        ;
    }
}

multi postcircumfix:<{ }> (VM $d, "name" )   {
    DEPRECATED('$*VM.name', :what('$*VM<name>') );
    $d.name
}
multi postcircumfix:<{ }> (VM $d, "config" ) {
    DEPRECATED('$*VM.config', :what('$*VM<config>') );
    $d.config
}
#?if jvm
multi postcircumfix:<{ }> (VM $d, "properties" ) {
    DEPRECATED('$*VM.properties', :what('$*VM<properties>') );
    $d.properties
}
#?endif

{
    my $name = # XXX: should be made dynamical
#?if parrot
      'parrot';
#?endif
#?if jvm
      'jvm';
#?endif
#?if moar
      'moar';
#?endif

    my $config :=
#?if parrot
      nqp::hllize(nqp::atpos(pir::getinterp__P,pir::const::IGLOBALS_CONFIG_HASH));
#?endif
#?if jvm
      do {
          my %CONFIG;
          my $jenv := nqp::backendconfig();
          my Mu $enviter := nqp::iterator($jenv);
          my $envelem;
          my $key;
          while $enviter {
              $envelem := nqp::shift($enviter);
              $key = nqp::p6box_s(nqp::iterkey_s($envelem));
              %CONFIG{$key} = nqp::p6box_s(nqp::iterval($envelem));
          }
          %CONFIG;
      };
#?endif
#?if moar
      nqp::backendconfig;
#?endif

#?if jvm
    my $properties = do {
        my %PROPS;
        my $jenv := nqp::jvmgetproperties();
        my Mu $enviter := nqp::iterator($jenv);
        my $envelem;
        my $key;
        while $enviter {
            $envelem := nqp::shift($enviter);
            $key = nqp::p6box_s(nqp::iterkey_s($envelem));
            %PROPS{$key} = nqp::p6box_s(nqp::iterval($envelem));
        }
        %PROPS;
    };
#?endif

    PROCESS::<$VM> = VM.new(
      :$name,
      :$config,
#?if jvm
      :$properties,
#?endif
    );
}

{
    ## duplicate src/core/IO.pm::cwd
    my $CWD = IO::Path.new(nqp::p6box_s(
#?if parrot
        pir::trans_encoding__Ssi(
            nqp::cwd(),
            pir::find_encoding__Is('utf8'))
#?endif
#?if !parrot
            nqp::cwd(),
#?endif
    ));
    nqp::bindkey(nqp::who(PROCESS), '$CWD', $CWD);

    my $PID = nqp::p6box_i(nqp::getpid());
    nqp::bindkey(nqp::who(PROCESS), '$PID', $PID);

    my $EXECUTABLE =
#?if parrot
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
#?endif
#?if jvm
        $*VM.properties<perl6.execname>
        or $*VM.properties<perl6.prefix> ~ '/bin/perl6-j';
#?endif
#?if moar
        nqp::execname()
        or ($*VM.config<prefix> ~ '/bin/' ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'));
#?endif
    $EXECUTABLE := $EXECUTABLE.path.absolute;
    PROCESS.WHO<$EXECUTABLE>      = $EXECUTABLE;
    PROCESS.WHO<$EXECUTABLE_NAME> = $EXECUTABLE.basename;

    my Mu $comp := nqp::getcomp('perl6');

    my $PROGRAM_NAME = $comp.user-progname();
    nqp::bindkey(nqp::who(PROCESS), '$PROGRAM_NAME', $PROGRAM_NAME);

    PROCESS::<$TMPDIR> = IO::Spec.tmpdir().path;
}
