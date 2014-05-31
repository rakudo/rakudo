# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic{
    has Str $.release;
    has Bool $.is-win;
    has Str $.path-sep;

    submethod BUILD (:$name, :$version, :$!release, :$!auth, :$!path-sep) {
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
#?if jvm
    my $properties := $*VM.properties;
    my $name       := $properties<os.name>;
    my $version    := $properties<os.version>;
    my $path-sep   := $properties<path.separator>;
#?endif
#?if !jvm
    my $config   := $*VM.config;
    my $name     := $config<osname>;
    my $version  := $config<osvers>;
    my $path-sep := $name eq 'MSWin32' ?? ';' !! ':';
#?endif
    my Str $release := "unknown";
    my Str $auth    := "unknown";

    # darwin specific info
    if $name eq 'darwin' {
        if qx/sw_vers/ ~~ m/ProductName\: \s+ (<[\w\ ]>+) \s+ ProductVersion\: \s+ (<[\d\.]>+) \s+ BuildVersion\: \s+ (<[\w\d]>+)/ {
            $name    := ~$0;
            $version := ~$1;
            $release := ~$2;
        }
        else {
            $name    := 'Mac OS X'; # we assume
            $version := "unknown";
            $release := "unknown";
        }
        $auth := 'Apple Computer, Inc.'; # presumably
    }

    # set up $*DISTRO and deprecated $*OS and $*OSVER
    PROCESS::<$DISTRO> =
      Distro.new( :$name, :$version, :$release, :$auth, :$path-sep );
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
