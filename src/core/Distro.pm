# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic{
    has Bool $.is-win;
    has Str $.release;

    submethod BUILD (:$name, :$version) {
        $!name    = $name.lc;
        $!auth    = "unknown";
        $!version = Version.new($version);
        $!is-win  = so $!name eq any <MSWin32 mingw msys cygwin>;
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

PROCESS::<$DISTRO> = Distro.new( :name($*OS), :version($*OSVER) );
$*OS = Deprecation.obsolete(
  :name('$*OS'), :value($*OS), :instead('$*DISTRO.name') );
$*OSVER = Deprecation.obsolete(
  :name('$*OSVER'), :value($*OSVER), :instead('$*DISTRO.version') );
