class Distro {
    has $.OS;
    has Bool $.is-win;

    submethod BUILD (:$!OS) {}

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

PROCESS::<$DISTRO> = Distro.new( :$*OS );
$*OS = Deprecation.obsolete(:name('$*OS'),:value($*OS),:instead('$*DISTRO.OS'));
