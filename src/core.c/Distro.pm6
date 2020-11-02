# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic {
    has Str $.release  is built(:bind);
    has Bool $.is-win  is built(False);
    has Str $.path-sep is built(:bind);

    submethod TWEAK (--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,$!name.lc.trans(" " => ""));  # lowercase spaceless
        $!is-win := so $!name eq any <mswin32 mingw msys cygwin>;
    }

    # This is a temporary migration method needed for installation
    method cur-sep() { "," }
}

# set up $*DISTRO
Rakudo::Internals.REGISTER-DYNAMIC: '$*DISTRO', {
#?if jvm
    my $properties := VM.new.properties;
    my $name       := $properties<os.name>;
    my $version    := $properties<os.version>;
    my $path-sep   := $properties<path.separator>;
#?endif
#?if !jvm
    my $config   := VM.new.config;
    my $name     := $config<osname>;
    my $version  := $config<osvers>;
    my $path-sep := $name eq 'MSWin32' ?? ';' !! ':';
#?endif
    my Str $release := "unknown";
    my Str $auth    := "unknown";
    my Str $desc    := "unknown";

    # helper sub to convert key:value lines into a hash
    sub kv2Map(Str:D $text, str $delimiter --> Map:D) {
        my $hash := nqp::hash;
        for $text.lines -> str $line {
            my $parts := nqp::split($delimiter,$line);
            if nqp::elems($parts) > 1 {
                nqp::bindkey(
                  $hash,
                  nqp::shift($parts),
                  nqp::hllize(
                    nqp::elems($parts) == 2
                      ?? nqp::shift($parts)
                      !! nqp::join($delimiter,$parts)
                  ).trim
                );
            }
        }

        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$hash)
    }

    # darwin specific info
    if $name eq 'darwin' {
        my $lookup :=
          kv2Map(shell("sw_vers", :out, :err).out.slurp(:close),':');
        $name    := $_ with $lookup<ProductName>;
        $version := $_ with $lookup<ProductVersion>;
        $release := $_ with $lookup<BuildVersion>;
        $auth    := 'Apple Inc.'; # presumably

#?if moar
        my constant $names = nqp::hash(
#?endif
#?if !moar
        my $names := nqp::hash(
#?endif
          '10.0',  'Cheetah',
          '10.1',  'Puma',
          '10.2',  'Jaguar',
          '10.3',  'Panther',
          '10.4',  'Tiger',
          '10.5',  'Leopard',
          '10.6',  'Snow Leopard',
          '10.7',  'Lion',
          '10.8',  'Mountain Lion',
          '10.9',  'Mavericks',
          '10.10', 'Yosemite',
          '10.11', 'El Capitan',
          '10.12', 'Sierra',
          '10.13', 'High Sierra',
          '10.14', 'Mojave',
          '10.15', 'Catalina',
          '11.0',  'Big Sur'
        );

        if nqp::atkey($names,$version.split(".").head(2).join(".")) -> $nick {
            $desc := $nick;
        }
    }
    elsif Rakudo::Internals.FILETEST-E('/etc/os-release') {
        my $lookup := kv2Map('/etc/os-release'.IO.slurp.subst(:g,'"'),'=');
        $name    := $_ with $lookup<ID>;
        $auth    := $_ with $lookup<HOME_URL>;
        $version := $_ with $lookup<VERSION>;
        $release := $_ with $lookup<VERSION_ID>;
        $desc    := $_ with $lookup<PRETTY_NAME>;
    }
    elsif $name eq 'linux' {
        my $lookup :=
          kv2Map(shell(<lsb_release -a>, :out, :err).out.slurp(:close),":");
        $auth    := $_ with $lookup<<"DISTRIBUTOR ID">>;
        $desc    := $_ with $lookup<DESCRIPTION>;
        $release := $_ with $lookup<RELEASE>;
    }

    $version := $version.Version;  # make sure it is a Version
    PROCESS::<$DISTRO> :=
      Distro.new(:$name, :$version, :$release, :$auth, :$path-sep, :$desc);
}

# vim: expandtab shiftwidth=4
