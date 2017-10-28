# An attempt at providing an API to nqp::getrusage.

class Telemetry::Period { ... }

class Telemetry {
    has int $!cpu-user;
    has int $!cpu-sys;
    has int $!wallclock;

    multi method new(Telemetry:) { nqp::create(self).SET-SELF }

    method SET-SELF() {
        my \rusage = nqp::getrusage;
        $!cpu-user = nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC);
        $!cpu-sys  = nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC);
        $!wallclock = nqp::fromnum_I(1000000 * nqp::time_n,Int);
        self
    }

    proto method cpu() { * }
    multi method cpu(Telemetry:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC)
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC)
    }
    multi method cpu(Telemetry:D:) is raw {
        nqp::add_i($!cpu-user,$!cpu-sys)
    }

    proto method cpu-user() { * }
    multi method cpu-user(Telemetry:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC)
    }
    multi method cpu-user(Telemetry:D:) is raw { $!cpu-user }

    proto method cpu-sys() { * }
    multi method cpu-sys(Telemetry:U:) is raw {
        my \rusage = nqp::getrusage;
        nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC)
    }
    multi method cpu-sys(Telemetry:D:) is raw { $!cpu-sys }

    proto method wallclock() { * }
    multi method wallclock(Telemetry:U:) is raw {
        nqp::fromnum_I(1000000 * nqp::time_n,Int)
    }
    multi method wallclock(Telemetry:D:) is raw { $!wallclock }

    proto method Period() { * }
    multi method Period(Telemetry:U \SELF:) is raw {
        if nqp::iscont(SELF) {
            SELF = SELF.new;
            nqp::create(Telemetry::Period)
        }
        else {
            die "Must use container of type Telemetry"
        }
    }
    multi method Period(Telemetry:D:) is raw {
        my int $cpu-user  = $!cpu-user;
        my int $cpu-sys   = $!cpu-sys;
        my int $wallclock = $!wallclock;
        self.SET-SELF;

        Telemetry::Period.new(
          nqp::sub_i($!cpu-user,$cpu-user),
          nqp::sub_i($!cpu-sys,$cpu-sys),
          nqp::sub_i($!wallclock,$wallclock)
        )
    }

    multi method Str(Telemetry:D:) {
        $!wallclock ?? "$.cpu / $!wallclock" !! "cpu / wallclock"
    }
    multi method gist(Telemetry:D:) {
        $!wallclock ?? "$.cpu / $!wallclock" !! "cpu / wallclock"
    }
}

class Telemetry::Period is Telemetry {
    multi method new(Telemetry::Period:
      int :$cpu-user,
      int :$cpu-sys,
      int :$wallclock
    ) {
        self.new($cpu-user, $cpu-sys, $wallclock)
    }
    multi method new(Telemetry::Period:
      int $cpu-user,
      int $cpu-sys,
      int $wallclock
    ) {
        my $period := nqp::create(Telemetry::Period);
        nqp::bindattr_i($period,Telemetry,'$!cpu-user', $cpu-user);
        nqp::bindattr_i($period,Telemetry,'$!cpu-sys',  $cpu-sys);
        nqp::bindattr_i($period,Telemetry,'$!wallclock',$wallclock);
        $period
    }

    multi method perl(Telemetry::Period:D:) {
        "Telemetry::Period.new(:cpu-user({
          nqp::getattr_i(self,Telemetry,'$!cpu-user')
        }), :cpu-sys({
          nqp::getattr_i(self,Telemetry,'$!cpu-sys')
        }), :wallclock({
          nqp::getattr_i(self,Telemetry,'$!wallclock')
        }))"
    }
}

multi sub infix:<->(Telemetry $a, Telemetry $b) {
    Telemetry::Period.new(
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!cpu-user'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!cpu-user')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!cpu-sys'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!cpu-sys')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!wallclock'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!wallclock')
      )
    )
}

# vim: ft=perl6 expandtab sw=4
