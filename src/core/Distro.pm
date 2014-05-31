# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic{
    has Bool $.is-win;
    has Str $.release;

    submethod BUILD (:$name, :$version, :$!release, :$!auth) {
        $!name = $name.lc;    # lowercase
        $!name ~~ s:g/" "//;  # spaceless
        $!version = Version.new($version);
        $!is-win  = so $!name eq any <mswin32 mingw msys cygwin>;
    }

    method release {
        $!release //= do {
            given $*DISTRO.name {
                when any <linux darwin> { # needs adapting
                    qx/uname -r/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }
}

{
    my $name =
#?if jvm
      $*VM.properties<os.name>;
#?endif
#?if !jvm
      $*VM.config<osname>;
#?endif

    my $version =
#?if jvm
      $*VM.properties<os.version>;
#?endif
#?if !jvm
      $*VM.config<osvers>;
#?endif
    my Str $release = "unknown";
    my Str $auth    = "unknown";

    # darwin specific info
    if $name eq 'darwin' {
        if qx/sw_vers/ ~~ m/ProductName\: \s+ (<[\w\ ]>+) \s+ ProductVersion\: \s+ (<[\d\.]>+) \s+ BuildVersion\: \s+ (<[\w\d]>+)/ {
            $name    = ~$0;
            $version = ~$1;
            $release = ~$2;
        }
        else {
            $name = 'Mac OS X'; # we assume
            $version = "unknown";
            $release = "unknown";
        }
        $auth = 'Apple Computer, Inc.'; # presumably
    }

    # set up $*DISTRO and deprecated $*OS and $*OSVER
    PROCESS::<$DISTRO> = Distro.new( :$name, :$version, :$release, :$auth );
    PROCESS::<$OS> = Deprecation.obsolete(
      :name('$*OS'),
      :value($name),
      :instead('$*DISTRO.name'),
    );
    PROCESS::<$OSVER> = Deprecation.obsolete(
      :name('$*OSVER'),
      :value($version),
      :instead('$*DISTRO.version'),
    );
}

# vim: ft=perl6 expandtab sw=4
