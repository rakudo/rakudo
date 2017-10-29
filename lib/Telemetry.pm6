# An attempt at providing an API to nqp::getrusage.

use nqp;

class Telemetry::Period { ... }

class Telemetry {
    has int $!cpu-user;
    has int $!cpu-sys;
    has int $!wallclock;

    my num $start = Rakudo::Internals.INITTIME;

    submethod BUILD() {
        my \rusage = nqp::getrusage;
        $!cpu-user = nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC);
        $!cpu-sys  = nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC);
        $!wallclock = nqp::fromnum_I(1000000*nqp::sub_n(nqp::time_n,$start),Int);
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
        nqp::fromnum_I(1000000 * nqp::sub_n(nqp::time_n,$start),Int)
    }
    multi method wallclock(Telemetry:D:) is raw { $!wallclock }

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

multi sub infix:<->(Telemetry:U $a, Telemetry:U $b) is export {
    Telemetry::Period.new(0,0,0)
}
multi sub infix:<->(Telemetry:D $a, Telemetry:U $b) is export { $a     - $b.new }
multi sub infix:<->(Telemetry:U $a, Telemetry:D $b) is export { $a.new - $b     }
multi sub infix:<->(Telemetry:D $a, Telemetry:D $b) is export {
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

my @snaps;
proto sub snap(|) is export { * }
multi sub snap(--> Nil) { @snaps.push(Telemetry.new) }
multi sub snap(@s --> Nil) { @s.push(Telemetry.new) }

proto sub periods(|) is export { * }
multi sub periods() {
    (1..^@snaps).map: {
        LAST @snaps = ();
        @snaps[$_] - @snaps[$_ - 1]
    }
}
multi sub periods(@s) { (1..^@s).map: { @s[$_] - @s[$_ - 1] } }

# vim: ft=perl6 expandtab sw=4
