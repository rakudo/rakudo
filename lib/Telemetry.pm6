# Provide an API for keeping track of a lot of system lifesigns

use nqp;

class Telemetry {
    has int $!cpu-user;
    has int $!cpu-sys;
    has int $!wallclock;
    has int $!supervisor;
    has int $!general-workers;
    has int $!general-tasks-queued;
    has int $!general-tasks-completed;
    has int $!timer-workers;
    has int $!timer-tasks-queued;
    has int $!timer-tasks-completed;
    has int $!affinity-workers;

    my num $start = Rakudo::Internals.INITTIME;

    sub completed(\workers) is raw {
        my int $elems = nqp::elems(workers);
        my int $completed;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::stmts(
            (my $w := nqp::atpos(workers,$i)),
            ($completed = nqp::add_i(
              $completed,
              nqp::getattr_i($w,$w.WHAT,'$!total')
            ))
          )
        );
        $completed
    }

    submethod BUILD() {
        my \rusage = nqp::getrusage;
        $!cpu-user = nqp::atpos_i(rusage,nqp::const::RUSAGE_UTIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC);
        $!cpu-sys  = nqp::atpos_i(rusage,nqp::const::RUSAGE_STIME_SEC) * 1000000
          + nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC);
        $!wallclock =
          nqp::fromnum_I(1000000 * nqp::sub_n(nqp::time_n,$start),Int);

        my $scheduler := nqp::decont($*SCHEDULER);
        $!supervisor = 1
          if nqp::getattr($scheduler,ThreadPoolScheduler,'$!supervisor');

        if nqp::getattr($scheduler,ThreadPoolScheduler,'$!general-workers')
          -> \workers {
            $!general-workers = nqp::elems(workers);
            $!general-tasks-completed = completed(workers);
        }
        if nqp::getattr($scheduler,ThreadPoolScheduler,'$!general-queue')
          -> \queue {
            $!general-tasks-queued = nqp::elems(queue);
        }
        if nqp::getattr($scheduler,ThreadPoolScheduler,'$!timer-workers')
          -> \workers {
            $!timer-workers = nqp::elems(workers);
            $!timer-tasks-completed = completed(workers);
        }
        if nqp::getattr($scheduler,ThreadPoolScheduler,'$!timer-queue')
          -> \queue {
            $!timer-tasks-queued = nqp::elems(queue);
        }
        if nqp::getattr($scheduler,ThreadPoolScheduler,'$!affinity-workers')
          -> \workers {
            $!affinity-workers = nqp::elems(workers);
        }

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

    proto method supervisor() { * }
    multi method supervisor(Telemetry:U:) {
        nqp::istrue(
          nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!supervisor'
          )
        )
    }
    multi method supervisor(Telemetry:D:) {
        $!supervisor
    }

    proto method general-workers() { * }
    multi method general-workers(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $workers := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-workers'
          ))),
          nqp::elems($workers)
        )
    }
    multi method general-workers(Telemetry:D:) {
        $!general-workers
    }

    proto method general-tasks-queued() { * }
    multi method general-tasks-queued(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $queue := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-queue'
          ))),
          nqp::elems($queue)
        )
    }
    multi method general-tasks-queued(Telemetry:D:) {
        $!general-tasks-queued
    }

    proto method general-tasks-completed() { * }
    multi method general-tasks-completed(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $workers := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-workers'
          ))),
          completed($workers)
        )
    }
    multi method general-tasks-completed(Telemetry:D:) {
        $!general-tasks-completed
    }

    proto method timer-workers() { * }
    multi method timer-workers(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $workers := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-workers'
          ))),
          nqp::elems($workers)
        )
    }
    multi method timer-workers(Telemetry:D:) { $!timer-workers }

    proto method timer-tasks-queued() { * }
    multi method timer-tasks-queued(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $queue := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-queue'
          ))),
          nqp::elems($queue)
        )
    }
    multi method timer-tasks-queued(Telemetry:D:) { $!timer-tasks-queued }

    proto method timer-tasks-completed() { * }
    multi method timer-tasks-completed(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $workers := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-workers'
          ))),
          completed($workers)
        )
    }
    multi method timer-tasks-completed(Telemetry:D:) {
        $!timer-tasks-completed
    }

    proto method affinity-workers() { * }
    multi method affinity-workers(Telemetry:U:) {
        nqp::if(
          nqp::istrue((my $workers := nqp::getattr(
            nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!affinity-workers'
          ))),
          nqp::elems($workers)
        )
    }
    multi method affinity-workers(Telemetry:D:) { $!affinity-workers }

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
      int :$wallclock,
      int :$supervisor,
      int :$general-workers,
      int :$general-tasks-queued,
      int :$general-tasks-completed,
      int :$timer-workers,
      int :$timer-tasks-queued,
      int :$timer-tasks-completed,
      int :$affinity-workers,
    ) {
        self.new(
          $cpu-user, $cpu-sys, $wallclock, $supervisor,
          $general-workers, $general-tasks-queued, $general-tasks-completed,
          $timer-workers, $timer-tasks-queued, $timer-tasks-completed,
          $affinity-workers
        )
    }
    multi method new(Telemetry::Period:
      int $cpu-user,
      int $cpu-sys,
      int $wallclock,
      int $supervisor,
      int $general-workers,
      int $general-tasks-queued,
      int $general-tasks-completed,
      int $timer-workers,
      int $timer-tasks-queued,
      int $timer-tasks-completed,
      int $affinity-workers,
    ) {
        my $period := nqp::create(Telemetry::Period);
        nqp::bindattr_i($period,Telemetry,
          '$!cpu-user',               $cpu-user);
        nqp::bindattr_i($period,Telemetry,
          '$!cpu-sys',                $cpu-sys);
        nqp::bindattr_i($period,Telemetry,
          '$!wallclock',              $wallclock);
        nqp::bindattr_i($period,Telemetry,
          '$!supervisor',             $supervisor);
        nqp::bindattr_i($period,Telemetry,
          '$!general-workers',        $general-workers);
        nqp::bindattr_i($period,Telemetry,
          '$!general-tasks-queued',   $general-tasks-queued);
        nqp::bindattr_i($period,Telemetry,
          '$!general-tasks-completed',$general-tasks-completed);
        nqp::bindattr_i($period,Telemetry,
          '$!timer-workers',          $timer-workers);
        nqp::bindattr_i($period,Telemetry,
          '$!timer-tasks-queued',     $timer-tasks-queued);
        nqp::bindattr_i($period,Telemetry,
          '$!timer-tasks-completed',  $timer-tasks-completed);
        nqp::bindattr_i($period,Telemetry,
          '$!affinity-workers',       $affinity-workers);
        $period
    }

    multi method perl(Telemetry::Period:D:) {
        "Telemetry::Period.new(:cpu-user({
          nqp::getattr_i(self,Telemetry,'$!cpu-user')
        }), :cpu-sys({
          nqp::getattr_i(self,Telemetry,'$!cpu-sys')
        }), :wallclock({
          nqp::getattr_i(self,Telemetry,'$!wallclock')
        }), :supervisor({
          nqp::getattr_i(self,Telemetry,'$!supervisor')
        }), :general-workers({
          nqp::getattr_i(self,Telemetry,'$!general-workers')
        }), :general-tasks-queued({
          nqp::getattr_i(self,Telemetry,'$!general-tasks-queued')
        }), :general-tasks-completed({
          nqp::getattr_i(self,Telemetry,'$!general-tasks-completed')
        }), :timer-workers({
          nqp::getattr_i(self,Telemetry,'$!timer-workers')
        }), :timer-tasks-queued({
          nqp::getattr_i(self,Telemetry,'$!timer-tasks-queued')
        }), :timer-tasks-completed({
          nqp::getattr_i(self,Telemetry,'$!timer-tasks-completed')
        }), :affinity-workers({
          nqp::getattr_i(self,Telemetry,'$!affinity-workers')
        }))"
    }

    method cpus() {
        nqp::add_i(
          nqp::getattr_i(self,Telemetry,'$!cpu-user'),
          nqp::getattr_i(self,Telemetry,'$!cpu-sys')
        ) / nqp::getattr_i(self,Telemetry,'$!wallclock')
    }

    my $factor = 100 / Kernel.cpu-cores;
    method utilization() { $factor * self.cpus }
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
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!supervisor'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!supervisor')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!general-workers'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!general-workers')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!general-tasks-queued'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!general-tasks-queued')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!general-tasks-completed'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!general-tasks-completed')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!timer-workers'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!timer-workers')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!timer-tasks-queued'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!timer-tasks-queued')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!timer-tasks-completed'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!timer-tasks-completed')
      ),
      nqp::sub_i(
        nqp::getattr_i(nqp::decont($a),Telemetry,'$!affinity-workers'),
        nqp::getattr_i(nqp::decont($b),Telemetry,'$!affinity-workers')
      )
    )
}

my @snaps;
proto sub snap(|) is export { * }
multi sub snap(--> Nil) { @snaps.push(Telemetry.new) }
multi sub snap(@s --> Nil) { @s.push(Telemetry.new) }

my int $snapper-running;
sub snapper($sleep = 0.1 --> Nil) is export {
    unless $snapper-running {
        snap;
        Thread.start(:app_lifetime, :name<Snapper>, {
            loop { sleep $sleep; snap }
        });
        $snapper-running = 1
    }
}

proto sub periods(|) is export { * }
multi sub periods() {
    my @s = @snaps;
    @snaps = ();
    @s.push(Telemetry.new) if @s == 1;
    periods(@s)
}
multi sub periods(@s) { (1..^@s).map: { @s[$_] - @s[$_ - 1] } }

proto sub report(|) is export { * }
multi sub report(:$legend, :$header-repeat = 32) {
    my $s := nqp::clone(nqp::getattr(@snaps,List,'$!reified'));
    nqp::setelems(nqp::getattr(@snaps,List,'$!reified'),0);
    nqp::push($s,Telemetry.new) if nqp::elems($s) == 1;
    report(
      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$s),
      :$legend,
      :$header-repeat,
    );
}

# Convert to spaces if numeric value is 0
sub hide0(\value, int $size = 3) {
    value ?? value.fmt("%{$size}d") !! nqp::x(" ",$size)
}

# Set up how to handle report generation
my %format =
  affinity-workers =>
    [     " aw", { hide0(.affinity-workers) },          '---',
      "The number of affinity threads"],
  cpu => 
    ["     cpu", { .cpu.fmt('%8d') },                   '--------',
      "The amount of CPU used (in microseconds)"],
  cpu-user => 
    ["cpu-user", { .cpu.fmt('%8d') },                   '--------',
      "The amount of CPU used in user code (in microseconds)"],
  cpu-sys =>               
    [" cpu-sys", { .cpu.fmt('%8d') },                   '--------',
      "The amount of CPU used in system overhead (in microseconds)"],
  general-workers =>
    [     " gw", { hide0(.general-workers) },           '---',
      "The number of general worker threads"],
  general-tasks-queued =>
    [     "gtq", { hide0(.general-tasks-queued) },      '---',
      "The number of tasks queued for execution in general worker threads"],
  general-tasks-completed =>
    [    " gtc", { hide0(.general-tasks-completed,4) }, '----',
      "The number of tasks completed in general worker threads"],
  supervisor =>
    [       "s", { hide0(.supervisor,1) },              '-',
      "The number of supervisors"],
  timer-workers =>
    [     " tw", { hide0(.timer-workers) },             '---',
      "The number of timer threads"],
  timer-tasks-queued =>
    [     "ttq", { hide0(.timer-tasks-queued) },        '---',
      "The number of tasks queued for execution in timer threads"],
  timer-tasks-completed =>
    [     "ttc", { hide0(.timer-tasks-completed) },     '---',
      "The number of tasks completed in timer threads"],
  utilization =>
    [  " util%", { .utilization.fmt('%6.2f') },         '------',
      "Percentage of CPU utilization (0..100%)"],
  wallclock =>
    ["wallclock", { .wallclock.fmt('%9d') },            '---------',
      "Number of microseconds elapsed"],
;

# Make sure we can also use the header key as an indicator
for %format.values -> \v {
    %format{v[0].trim} = v
}

multi sub report(
  @s,
  @cols = <wallclock util% gw gtc tw ttc aw>,
  :$legend,
  :$header-repeat = 32,
) {

    my $total = @s[*-1] - @s[0];
    my $text := nqp::list_s(qq:to/HEADER/.chomp);
Telemetry Report of Process #$*PID ({Instant.from-posix(nqp::time_i).DateTime})
Number of Snapshots: {+@s}
Total Time:      { ($total.wallclock / 1000000).fmt('%9.2f') } seconds
Total CPU Usage: { ($total.cpu / 1000000).fmt('%9.2f') } seconds
HEADER

    sub push-period($period) {
        nqp::push_s($text,
          %format{@cols}>>.[1]>>.($period).join(' ').trim-trailing);
    }

    my $header = "\n%format{@cols}>>.[0].join(' ')";
    nqp::push_s($text,$header) unless $header-repeat;

    for periods(@s).kv -> $index, $period {
        nqp::push_s($text,$header)
          if $header-repeat && $index %% $header-repeat;
        push-period($period)
    }

    nqp::push_s($text,%format{@cols}>>.[2].join(' '));

    push-period($total);

    if $legend {
        nqp::push_s($text,'');
        nqp::push_s($text,'Legend:');
        for %format{@cols} -> $col {
            nqp::push_s($text," $col[0].trim-leading.fmt('%5s')  $col[3]");
        }
    }

    nqp::join("\n",$text)
}

END { if @snaps { snap; note report } }

# vim: ft=perl6 expandtab sw=4
