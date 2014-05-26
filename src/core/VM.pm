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

# vim: ft=perl6 expandtab sw=4
