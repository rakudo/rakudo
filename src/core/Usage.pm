# An attempt at providing an API to nqp::getrusage.

class Usage {
    has int $!cpu-user;
    has int $!cpu-sys;

    submethod BUILD(--> Nil) {
        my \rusage = nqp::getrusage;
        $!cpu-user = nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC);
        $!cpu-sys  = nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC);
    }

    proto method cpu() { * }
    multi method cpu(Usage:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC)
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC)
    }
    multi method cpu(Usage:D:) is raw {
        nqp::add_i($!cpu-user,$!cpu-sys)
    }

    proto method cpu-user() { * }
    multi method cpu-user(Usage:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC)
    }
    multi method cpu-user(Usage:D:) is raw { $!cpu-user }

    proto method cpu-sys() { * }
    multi method cpu-sys(Usage:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC)
    }
    multi method cpu-sys(Usage:D:) is raw { $!cpu-sys }

    proto method interval() { * }
    multi method interval(Usage:U \SELF:) is raw { SELF = SELF.new; 0 }
    multi method interval(Usage:D \SELF:) is raw {
        my int $cpu = (my $new := Usage.new) - SELF;
        SELF = $new;
        $cpu;
    }
}

multi sub infix:<->(Usage $a, Usage $b) {
    $a.cpu - $b.cpu
}

# vim: ft=perl6 expandtab sw=4
