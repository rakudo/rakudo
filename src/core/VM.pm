class VM does Systemic {
    has $.config;
#?if jvm
    has $.properties;
#?endif
    has $.precomp-ext;
    has $.precomp-target;
    has $.prefix;

    submethod BUILD (
      :$!config,
#?if jvm
      :$!properties,
#?endif
    ) {
#?if parrot
        $!name           = 'parrot';
        $!auth           = "Parrot Foundation";
        $!version        = Version.new($!config<VERSION> // "unknown");
        $!precomp-ext    = "pir";
        $!precomp-target = "pir";
        $!prefix         = $!config<libdir> ~ $!config<versiondir>;
#?endif
#?if jvm
        $!name           = 'jvm';
        $!auth           = $!properties<java.vendor> // "unknown";
        $!version        = Version.new($!properties<java.specification.version> // "unknown");
        $!precomp-ext    = "jar";
        $!precomp-target = "jar";
        $!prefix         = $!properties<perl6.prefix>;
#?endif
#?if moar
        $!name           = 'moar';
        $!auth           = "The MoarVM Team";
        $!version        = Version.new($!config<version> // "unknown");
        $!precomp-ext    = "moarvm";
        $!precomp-target = "mbc";
        $!prefix         = $!config<prefix>;
#?endif
# add new backends here please
    }
}

multi sub postcircumfix:<{ }> (VM $d, "name" )   {
    DEPRECATED('$*VM.name', |<2014.05 2015.05>, :what('$*VM<name>') );
    $d.name
}
multi sub postcircumfix:<{ }> (VM $d, "config" ) {
    DEPRECATED('$*VM.config', |<2014.05 2015.05>, :what('$*VM<config>') );
    $d.config
}
#?if jvm
multi sub postcircumfix:<{ }> (VM $d, "properties" ) {
    DEPRECATED('$*VM.properties', |<2014.05 2015.05>, :what('$*VM<properties>') );
    $d.properties
}
#?endif

multi sub INITIALIZE_DYNAMIC('$*VM') {
    PROCESS::<$VM> := do {
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
        my $properties := do {
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
        VM.new(
            :$config,
#?if jvm
            :$properties,
#?endif
        );
    };
}

# vim: ft=perl6 expandtab sw=4
