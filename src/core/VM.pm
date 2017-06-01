my constant $?COMPILATION-ID :=
  nqp::sha1(nqp::concat(
    $*W.handle,
    nqp::getcomp('perl6').compilation-id
  ));

class VM does Systemic {
    has $.config;
#?if jvm
    has $.properties;
#?endif
    has $.prefix;
    has $.precomp-ext;
    has $.precomp-target;

    submethod BUILD(
      :$!config,
      :$!desc = Str,
#?if jvm
      :$!properties,
#?endif
      --> Nil
    ) {
#?if moar
        $!name           = 'moar';
        $!auth           = "The MoarVM Team";
        $!version        = Version.new($!config<version> // "unknown");
        $!prefix         = $!config<prefix>;
        $!precomp-ext    = "moarvm";
        $!precomp-target = "mbc";
#?endif
#?if jvm
        $!name           = 'jvm';
        $!auth           = $!properties<java.vendor> // "unknown";
        $!version        = Version.new($!properties<java.specification.version> // "unknown");
        $!prefix         = $!properties<perl6.prefix>;
        $!precomp-ext    = "jar";
        $!precomp-target = "jar";
        $!config<os.name> = $!properties<os.name> // "unknown";
#?endif
#?if js
        $!name           = 'js';
        $!auth           = "unknown";
        $!version        = Version.new("unknown");
        $!prefix         = 'todo';
        $!precomp-ext    = "todo";
        $!precomp-target = "todo";
#?endif
# add new backends here please
    }

    method platform-library-name(IO::Path $library, Version :$version) {
        my int $is-win = Rakudo::Internals.IS-WIN;
        my int $is-darwin = self.osname eq 'darwin';

        my $basename  = $library.basename;
        my int $full-path = $library ne $basename;
        my $dirname   = $library.dirname;

        # OS X needs version before extension
        $basename ~= ".$version" if $is-darwin && $version.defined;

#?if moar
        my $dll = self.config<dll>;
        my $platform-name = sprintf($dll, $basename);
#?endif
#?if jvm
        my $prefix = $is-win ?? '' !! 'lib';
        my $platform-name = "$prefix$basename" ~ ".{self.config<nativecall.so>}";
#?endif
#?if js
        my $prefix = 'todo';
        my $platform-name = 'todo';
#?endif

        $platform-name ~= '.' ~ $version
            if $version.defined and nqp::iseq_i(nqp::add_i($is-darwin,$is-win),0);

        $full-path
          ?? $dirname.IO.add($platform-name).absolute
          !! $platform-name.IO
    }

    proto method osname(|) { * }
    multi method osname(VM:U:) {
#?if jvm
        nqp::lc(nqp::atkey(nqp::jvmgetproperties,'os.name'))
#?endif
#?if !jvm
        nqp::lc(nqp::atkey(nqp::backendconfig,'osname'))
#?endif
    }
    multi method osname(VM:D:) {
#?if jvm
        nqp::lc($!properties<os.name>)
#?endif
#?if !jvm
        nqp::lc($!config<osname>)
#?endif
    }
}

sub INITIALIZE-A-VM-NOW() {
    my $desc := DateTime.now.Str;
#?if !jvm
    VM.new(:config(nqp::backendconfig),:$desc);
#?endif
#?if jvm
    my $config := do {
        my %CONFIG;
        my $jenv := nqp::backendconfig();
        my Mu $enviter := nqp::iterator($jenv);
        my $key;
        while $enviter {
            $key = nqp::p6box_s(nqp::iterkey_s(nqp::shift($enviter)));
            %CONFIG{$key} = nqp::p6box_s(nqp::iterval($enviter));
        }
        %CONFIG;
    }
    my $properties := do {
        my %PROPS;
        my $jenv := nqp::jvmgetproperties();
        my Mu $enviter := nqp::iterator($jenv);
        my $key;
        while $enviter {
            $key = nqp::p6box_s(nqp::iterkey_s(nqp::shift($enviter)));
            %PROPS{$key} = nqp::p6box_s(nqp::iterval($enviter));
        }
        %PROPS;
    }
    VM.new(:$config,:$desc,:$properties);
#?endif
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*VM', {
    PROCESS::<$VM> := INITIALIZE-A-VM-NOW();
}

# vim: ft=perl6 expandtab sw=4
