my constant $?COMPILATION-ID :=
  nqp::p6box_s(nqp::sha1(nqp::concat(
    $*W.handle,
    nqp::getcomp('Raku').compilation-id
  )));

class VM does Systemic {
#?if moar
    has $.config         is built(:bind) = nqp::backendconfig;
    has $.prefix         is built(:bind) = $!config<prefix>;
    has $.precomp-ext    is built(:bind) = "moarvm";
    has $.precomp-target is built(:bind) = "mbc";
#?endif
#?if jvm
    has $.config         is built(:bind) = default-JVM-config;
    has $.properties     is built(:bind) = default-JVM-properties;
    has $.prefix         is built(:bind) = $!properties<perl6.prefix>;
    has $.precomp-ext    is built(:bind) = "jar";
    has $.precomp-target is built(:bind) = "jar";
#?endif
#?if js
    has $.config         is built(:bind) = nqp::backendconfig;
    has $.prefix         is built(:bind) = 'todo-prefix';
    has $.precomp-ext    is built(:bind) = "js";
    has $.precomp-target is built(:bind) = "js";
#?endif

    submethod TWEAK(--> Nil) {
#?if moar
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'moar');
        nqp::bind($!desc,'Short for "Metamodel On A Runtime", MoarVM is a modern virtual machine built for the Rakudo compiler and the NQP Compiler Toolchain.');
        nqp::bind($!auth,'The MoarVM Team');
        nqp::bind($!version,Version.new($!config<version> // "unknown"));
#?endif
#?if jvm
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'jvm');
        nqp::bind($!desc,'The Java Virtual Machine');
        nqp::bind($!auth,$!properties<java.vendor> // 'unknown');
        nqp::bind($!version,Version.new($!properties<java.specification.version> // "unknown"));
        $!config<os.name> := $!properties<os.name> // 'unknown';
#?endif
#?if js
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'js');
        nqp::bind($!desc,'JavaScript');
        nqp::bind($!auth,'unknown');
        nqp::bind($!version,Version.new($!config<version> // 'unknown'));
#?endif
# add new backends here please
    }

#?if jvm
    sub default-JVM-config(--> Hash) {
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
    sub default-JVM-properties(--> Hash) {
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
#?endif

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

    method request-garbage-collection(--> Nil) {
#?if moar
        nqp::force_gc
#?endif
#?if !moar
        warn "Requesting garbage collection not supported on this backend";
#?endif
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*VM', {
    PROCESS::<$VM> := VM.new;
}

# vim: expandtab shiftwidth=4
