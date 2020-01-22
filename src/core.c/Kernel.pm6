# The Kernel class and its methods, underlying $*KERNEL, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Kernel does Systemic {
    has Str $.release;
    has Str $!hardware;
    has Str $!arch;
    has Int $!bits;

#?if !jvm
    has $!uname;
    method !uname {
        $!uname ?? $!uname !! ($!uname := nqp::uname())
    }
#?endif

    method !uname-s {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_SYSNAME)
#?endif
#?if jvm
        try shell('uname -s', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-r {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_RELEASE)
#?endif
#?if jvm
        try shell('uname -r', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-v {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_VERSION)
#?endif
#?if jvm
        try shell('uname -v', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-m {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_MACHINE)
#?endif
#?if jvm
        try shell('uname -m', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-p {
        # TODO: find a way to get this without shelling out
        try shell("uname -p", :out, :!err).out.slurp(:close).chomp;
    }

    submethod BUILD(:$!auth = 'unknown' --> Nil) { }

    method name {
        $!name //= do {
            given $*DISTRO.name {
                when 'mswin32' {
                    'win32'
                }
                when 'browser' {
                    'browser';
                }
                default {
                    lc self!uname-s();
                }
            }
        }
    }

    method version {
        # it doesn't make sense to return a Version object here, but its currently enforced by roast
        # TODO: remove Version checks from roast? and check ecosystem for fallout.
        $!version //= Version.new(self!uname-v());
    }

    method release {
        # somewhat counter-intuitively the UNAME_RELEASE is what
        # most people think of the kernel version
        $!release //= self!uname-r();
    }

    method hardware {
        $!hardware //= self!uname-m();
    }

    method arch {
        $!arch //= do {
            given $*DISTRO.name {
                when 'raspbian' {
                    self!uname-m();
                }
                when 'browser' {
                    self!uname-m();
                }
                default {
                    self!uname-p();
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

    method hostname {
        nqp::p6box_s(nqp::gethostname)
    }

    has @!signals;  # Signal
#?if jvm
    method signals (Kernel:D:) {
        @!signals //= [2, 9]
    }
#?endif

    has $!signals-setup-lock = Lock.new;
#?if !jvm
    has $!signals-setup = False;
    method signals (Kernel:D:) {
        unless $!signals-setup {
            $!signals-setup-lock.protect: {
                unless $!signals-setup {
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
                    $!signals-setup = True;
                }
            }
        }
        @!signals
    }
#?endif

    has %!signals-by-Str;
    has $!signals-by-Str-setup = False;

    proto method signal (|) {*}
    multi method signal(Kernel:D: Str:D $signal --> Int:D) {
        unless $!signals-by-Str-setup {
            $!signals-setup-lock.protect: {
                unless $!signals-by-Str-setup {
                    nqp::stmts(
                      (my int $els = @.signals.elems),
                      (my int $i = -1),
                      nqp::while(
                        nqp::isgt_i($els, $i = nqp::add_i($i, 1)),
                        ($_ := @!signals.AT-POS($i)).defined
                          && %!signals-by-Str.ASSIGN-KEY(.Str, nqp::decont($i))));
                    $!signals-by-Str-setup = True;
                }
            }
        }
        %!signals-by-Str{$signal} // %!signals-by-Str{"SIG$signal"} // Int;
    }

    multi method signal(Kernel:D: Signal:D \signal --> Int:D) { signal.value }
    multi method signal(Kernel:D: Int:D    \signal --> Int:D) { signal       }

    method cpu-cores(--> Int) is raw {
        nqp::cpucores()
    }

    method cpu-usage(--> Int) is raw {
        my int @rusage;
        nqp::getrusage(@rusage);
        nqp::atpos_i(@rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(@rusage, nqp::const::RUSAGE_UTIME_MSEC)
          + nqp::atpos_i(@rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(@rusage, nqp::const::RUSAGE_STIME_MSEC)
    }

    method free-memory(--> Int) {
        nqp::freemem()
    }

    my $total-mem := nqp::null();
    method total-memory(--> Int) {
        nqp::ifnull(
          $total-mem,
          nqp::bind($total-mem,nqp::p6box_i(nqp::totalmem()))
        )
    }

    my $endian := nqp::null;
    method endian(--> Endian:D) {
        nqp::ifnull(
          $endian,
          nqp::bind($endian,nqp::if(
            blob8.new(0,1).read-int16(0) == 1,  # hacky way to find out
            BigEndian,
            LittleEndian
          ))
        )
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*KERNEL', {
    PROCESS::<$KERNEL> := Kernel.new;
}

# vim: ft=perl6 expandtab sw=4
