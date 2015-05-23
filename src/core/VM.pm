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
      :$!desc = Str,
#?if jvm
      :$!properties,
#?endif
    ) {
#?if moar
        $!name           = 'moar';
        $!auth           = "The MoarVM Team";
        $!version        = Version.new($!config<version> // "unknown");
        $!precomp-ext    = "moarvm";
        $!precomp-target = "mbc";
        $!prefix         = $!config<prefix>;
#?endif
#?if jvm
        $!name           = 'jvm';
        $!auth           = $!properties<java.vendor> // "unknown";
        $!version        = Version.new($!properties<java.specification.version> // "unknown");
        $!precomp-ext    = "jar";
        $!precomp-target = "jar";
        $!prefix         = $!properties<perl6.prefix>;
#?endif
# add new backends here please
    }
}

sub INITIALIZE-A-VM-NOW() {
    my $desc := DateTime.now.Str;
#?if moar
    VM.new(:config(nqp::backendconfig),:$desc);
#?endif
#?if jvm
    my $config := do {
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
    }
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
    }
    VM.new(:$config,:$desc,:$properties);
#?endif
}

multi sub INITIALIZE_DYNAMIC('$*VM') {
    PROCESS::<$VM> := INITIALIZE-A-VM-NOW();
}

# vim: ft=perl6 expandtab sw=4
