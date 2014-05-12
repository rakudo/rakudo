class Distro {
    has $.name;
    has $.version;
    has $.is-win;

    submethod BUILD (:$!name, :$!version) {
        $!version = "unknown" if $!name eq $!version;
        $!is-win  = so $!name eq any <MSWin32 mingw msys cygwin>;
    }

#?if moar
    has @!signals;  # Signal
    method signals (Distro:D:) {
        once {
            my @names = "",qx/kill -l/.words;
            @names.splice(1,1) if @names[1] eq "0";  # Ubuntu fudge

            for Signal.^enum_value_list -> $signal {
                my $name = $signal.key.substr(3);
                if @names.first-index( * eq $name ) -> $index {
                    @!signals[$index] = $signal;
                }
            }
        }
        @!signals
    }
#?endif
}

PROCESS::<$DISTRO> = Distro.new( :name($*OS), :version($*OSVER) );
$*OS = Deprecation.obsolete(
  :name('$*OS'), :value($*OS), :instead('$*DISTRO.name') );
$*OSVER = Deprecation.obsolete(
  :name('$*OSVER'), :value($*OSVER), :instead('$*DISTRO.version') );
