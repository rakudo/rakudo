# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro {
    has $.name;
    has $.ver;
    has $.is-win;

    submethod BUILD (:$!name, :$!ver) {
        $!ver = "unknown" if $!name eq $!ver;
        $!is-win  = so $!name eq any <MSWin32 mingw msys cygwin>;
    }
    method gist { $!name ~ (" ($!ver)" if $!ver ne "unknown") }
    method Str  { $!name }
}

PROCESS::<$DISTRO> = Distro.new( :name($*OS), :ver($*OSVER) );
$*OS = Deprecation.obsolete(
  :name('$*OS'), :value($*OS), :instead('$*DISTRO.name') );
$*OSVER = Deprecation.obsolete(
  :name('$*OSVER'), :value($*OSVER), :instead('$*DISTRO.ver') );
