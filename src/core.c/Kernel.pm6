# The Kernel class and its methods, underlying $*KERNEL, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Kernel does Systemic {
    has Str $!release  is built(:bind);
    has Str $!hardware is built(:bind);
    has Str $!arch     is built(:bind);
    has Int $!bits     is built(:bind);

#?if !jvm
    has $!uname;
    method !uname {
        $!uname ?? $!uname !! ($!uname := nqp::uname())
    }
#?endif

    method !uname-s(--> Str:D) {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_SYSNAME)
#?endif
#?if jvm
        try shell('uname -s', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-r(--> Str:D) {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_RELEASE)
#?endif
#?if jvm
        try shell('uname -r', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-v(--> Str:D) {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_VERSION)
#?endif
#?if jvm
        try shell('uname -v', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-m(--> Str:D) {
#?if !jvm
        nqp::atpos_s(self!uname, nqp::const::UNAME_MACHINE)
#?endif
#?if jvm
        try shell('uname -m', :out, :!err).out.slurp(:close).chomp;
#?endif
    }

    method !uname-p(--> Str:D) {
        # TODO: find a way to get this without shelling out
        try shell("uname -p", :out, :!err).out.slurp(:close).chomp;
    }

    method name(--> Str:D) {
        $!name eq 'unknown' ?? self!name($*DISTRO.name) !! $!name
    }
    method !name(Str:D \distro --> Str:D) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,distro eq 'mswin32'
          ?? 'win32'
          !! distro eq 'browser'
            ?? 'browser'
            !! self!uname-s.lc
        )
    }

    method version(--> Version:D) {
        $!version
          ?? $!version
          # https://github.com/rakudo/rakudo/issues/3436
          !! nqp::bind($!version,self!uname-v.Version)
    }

    method release(--> Str:D) {
        # somewhat counter-intuitively the UNAME_RELEASE is what
        # most people think of the kernel version
        $!release ?? $!release !! ($!release := self!uname-r)
    }

    method hardware(--> Str:D) {
        $!hardware ?? $!hardware !! ($!hardware := self!uname-m)
    }

    method arch {
        $!arch ?? $!arch !! self!arch($*DISTRO.name)
    }
    method !arch(Str:D \distro --> Str:D) {
        $!arch := distro eq 'raspbian'
          ?? self!uname-m
          !! distro eq 'browser'
            ?? self!uname-m
            !! self!uname-p
    }

    method archname(--> Str:D) {
        self.hardware ~ '-' ~ self.name
    }

    method bits(--> Int:D) {
        $!bits
          ?? $!bits
          # naive approach
          !! ($!bits := $.hardware ~~ m/_64|w|amd64/ ?? 64 !! 32);
    }

    method hostname(--> Str:D) {
        nqp::p6box_s(nqp::gethostname)
    }

    has @!signals;  # Signal
    has $!signals-setup-lock = Lock.new;
    has $!signals-setup      = False;

    method signals (Kernel:D:) {
        unless $!signals-setup {
            $!signals-setup-lock.protect: {
                unless $!signals-setup {
                    my \arr = nqp::list(Nil);
                    my int $els = nqp::add_i(Signal.enums.values.max, 1);
                    my int $i   = 1;

                    nqp::while(
                      nqp::islt_i($i, $els),
                      nqp::bindpos(arr, $i, Signal($i) // Nil),
                      $i = nqp::add_i($i, 1),
                    );
                    @!signals       = |arr;
                    $!signals-setup = True;
                }
            }
        }
        @!signals
    }

    has %!signals-by-Str;
    has $!signals-by-Str-setup = False;

    proto method signal (|) {*}
    multi method signal(Kernel:D: Str:D $signal --> Int:D) {
        unless $!signals-by-Str-setup {
            $!signals-setup-lock.protect: {
                unless $!signals-by-Str-setup {
                    my int $els = @.signals.elems;
                    my int $i = -1;

                    nqp::while(
                      nqp::isgt_i($els, $i = nqp::add_i($i, 1)),
                      ($_ := @!signals.AT-POS($i)).defined
                        && %!signals-by-Str.ASSIGN-KEY(.Str, nqp::decont($i))
                    );
                    $!signals-by-Str-setup := True;
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

# vim: expandtab shiftwidth=4
