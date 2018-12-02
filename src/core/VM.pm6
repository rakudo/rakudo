my constant $?COMPILATION-ID :=
  nqp::p6box_s(nqp::sha1(nqp::concat(
    $*W.handle,
    nqp::getcomp('perl6').compilation-id
  )));

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
      :$desc,
#?if jvm
      :$!properties,
#?endif
      --> Nil
    ) {
#?if moar
        $!name           = 'moar';
        $!desc           = $desc // 'Short for "Metamodel On A Runtime", MoarVM is a modern virtual machine built for the Rakudo Perl 6 compiler and the NQP Compiler Toolchain.';
        $!auth           = "The MoarVM Team";
        $!version        = Version.new($!config<version> // "unknown");
        $!prefix         = $!config<prefix>;
        $!precomp-ext    = "moarvm";
        $!precomp-target = "mbc";
#?endif
#?if jvm
        $!name           = 'jvm';
        $!desc           = $desc // 'The Java Virtual Machine';
        $!auth           = $!properties<java.vendor> // "unknown";
        $!version        = Version.new($!properties<java.specification.version> // "unknown");
        $!prefix         = $!properties<perl6.prefix>;
        $!precomp-ext    = "jar";
        $!precomp-target = "jar";
        $!config<os.name> = $!properties<os.name> // "unknown";
#?endif
#?if js
        $!name           = 'js';
        $!desc           = $desc // 'JavaScript';
        $!auth           = "unknown";
        $!version        = Version.new("unknown");
        $!prefix         = 'todo-prefix';
        $!precomp-ext    = "js";
        $!precomp-target = "js";
#?endif
# add new backends here please
    }

    method platform-library-name(IO::Path $library, Version :$version) {
        my int $is-win = Rakudo::Internals.IS-WIN;
        my int $is-darwin = self.osname eq 'darwin';
        my int $is-openbsd = self.osname eq 'openbsd';

        my $basename  = $library.basename;
        my int $full-path = $library ne $basename;
        my $dirname   = $library.dirname;

        # OS X needs version before extension
        $basename ~= ".$version" if $is-darwin && $version.defined;

#?if moar
        my $dll = self.config<dll>;
        my $platform-name = sprintf($dll, $basename);
#?endif
#?if !moar
        my $prefix = $is-win ?? '' !! 'lib';
        my $platform-name = "$prefix$basename" ~ ".{self.config<nativecall.so>}";
#?endif

        $platform-name ~= '.' ~ $version
            if $version.defined and nqp::iseq_i(nqp::add_i(nqp::add_i($is-darwin,$is-win),$is-openbsd),0);

        $full-path
          ?? $dirname.IO.add($platform-name).absolute
          !! $platform-name.IO
    }

    proto method osname(|) {*}
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
#?if !jvm
    VM.new(:config(nqp::backendconfig));
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
    VM.new(:$config,:$properties);
#?endif
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*VM', {
    PROCESS::<$VM> := INITIALIZE-A-VM-NOW();
}

# vim: ft=perl6 expandtab sw=4
