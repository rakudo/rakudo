# The Kernel class and its methods, underlying $*KERNEL, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Kernel does Systemic {
    has Str $.release;
    has Str $!hardware;
    has Str $!arch;
    has Int $!bits;

    sub uname($opt) {
        state $has_uname = "/bin/uname".IO.x || "/usr/bin/uname".IO.x;
        $has_uname ?? qqx/uname $opt/.chomp !! 'unknown';
    }

    submethod BUILD(:$!auth = "unknown" --> Nil) { }

    method name {
        $!name //= do {
            given $*DISTRO.name {
                when 'mswin32' {
                    'win32'
                }
                default {
                    lc uname '-s';
                }
            }
        }
    }

    method version {
        $!version //= Version.new( do {
            given $*DISTRO.name {
                when 'freebsd' {
                    uname '-r'; # -K -U not introduced until 10.0
                }
                when 'macosx' {
                    my $unamev = uname '-v';
                    $unamev ~~ m/^Darwin \s+ Kernel \s+ Version \s+ (<[\d\.]>+)/
                      ?? ~$0
                      !! $unamev.chomp;
                }
                default {
                    given $.name {
                        when 'linux' {
                            # somewhat counter-intuitively the '-r' is what
                            # most people think of the kernel version
                            uname '-r';
                        }
                        default {
                            uname '-v';
                        }
                    }
                }
            }
        } );
    }

    method release {
        $!release //= do {
            given $*DISTRO.name {
                when any <openbsd netbsd dragonfly> { # needs adapting
                    uname '-r';
                }
                default {
                    uname '-v';
                }
            }
        }
    }

    method hardware {
        $!hardware //= do {
            given $*DISTRO.name {
                default {
                    uname '-m';
                }
            }
        }
    }

    method arch {
        $!arch //= do {
            given $*DISTRO.name {
                when 'raspbian' {
                    uname '-m';
                }
                default {
                    uname '-p';
                }
            }
        }
    }

    method archname {
        self.hardware ~ '-' ~ self.name
    }

    method bits {
        $!bits //= $.hardware ~~ m/_64|w|amd64/ ?? 64 !! 32;  # naive approach
    }

    has @!signals;  # Signal
#?if jvm
    method signals (Kernel:D:) {
        @!signals //= [2, 9]
    }
#?endif
#?if moar
    method signals (Kernel:D:) {
        once {
            my @names;
            if self.name eq 'win32' {
                # These are the ones libuv emulates on Windows.
                @names = flat "", <INT BREAK HUP WINCH>;
            } else {
                if self.name eq 'openbsd' {
                    # otherwise it uses a shell buildin
                    @names = flat "", qx!/bin/kill -l!.words;
                }
                else {
                    @names = flat "", qx/kill -l/.words;
                }
                @names.splice(1,1) if @names[1] eq "0";  # Ubuntu fudge
                @names.=map({.uc}) if $*KERNEL.name eq 'dragonfly';
            }

            for Signal.^enum_value_list -> $signal {
                my $name = substr($signal.key,3);
                if @names.first( * eq $name, :k ) -> $index {
                    @!signals[$index] = $signal;
                }
            }
        }
        @!signals
    }
#?endif

    has %!signals_by_Str;
    proto method signal (|) { * }
    multi method signal(Kernel:D: Str:D $signal --> Int:D) {

    # NOTE: if you make this method thread-safe, remove the locking
    # done by Proc::Async.kill

        once {
            nqp::stmts(
              (my int $els = @.signals.elems),
              (my int $i = -1),
              nqp::while(
                nqp::isgt_i($els, $i = nqp::add_i($i, 1)),
                ($_ := @!signals.AT-POS($i)).defined
                  && %!signals_by_Str.ASSIGN-KEY(.Str, nqp::decont($i))))
        }
        %!signals_by_Str{$signal} // %!signals_by_Str{"SIG$signal"} // Int;
    }

    multi method signal(Kernel:D: Signal:D \signal --> Int:D) { signal.value }
    multi method signal(Kernel:D: Int:D    \signal --> Int:D) { signal       }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*KERNEL', {
    PROCESS::<$KERNEL> := Kernel.new;
}

# vim: ft=perl6 expandtab sw=4
