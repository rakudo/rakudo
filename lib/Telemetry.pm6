# Provide an API for keeping track of a lot of system lifesigns

use nqp;

# Constants indexing into the nqp::getrusage array -----------------------------
constant UTIME_SEC  =  0;
constant UTIME_MSEC =  1;
constant STIME_SEC  =  2;
constant STIME_MSEC =  3;
constant MAX_RSS    =  4;
constant IX_RSS     =  5;
constant ID_RSS     =  6;
constant IS_RSS     =  8;
constant MIN_FLT    =  9;
constant MAJ_FLT    = 10;
constant NSWAP      = 11;
constant INBLOCK    = 12;
constant OUTBLOCK   = 13;
constant MSGSND     = 14;
constant MSGRCV     = 14;
constant NSIGNALS   = 15;
constant NVCSW      = 16;
constant INVCSW     = 17;
constant RUSAGE_ELEMS = 18;

# Helper stuff -----------------------------------------------------------------
my num $start = Rakudo::Internals.INITTIME;
my int $b2kb  = nqp::atkey(nqp::backendconfig,q/osname/) eq 'darwin' ?? 10 !! 0;

# calculate number of tasks completed for a worker list
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

# calculate number of tasks queued for an affinity worker list, which has
# a separate queue for each worker
sub queued(\workers) is raw {
    my int $elems = nqp::elems(workers);
    my int $queued;
    my int $i = -1;
    nqp::while(
      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
      nqp::stmts(
        (my $w := nqp::atpos(workers,$i)),
        ($queued = nqp::add_i(
          $queued,
          nqp::elems(nqp::getattr($w,$w.WHAT,'$!queue'))
        ))
      )
    );
    $queued
}

# usable names of attributes that are part of getrusage struct
constant @rusage_names = << "" "" "" "" # first 4 are special
  max-rss ix-rss   id-rss is-rss min-flt  maj-flt nswap
  inblock outblock msgsnd msgrcv nsignals nvcsw   invcsw
>>;

# names of attributes that are native integers
constant @scheduler_names = <
  $!supervisor
  $!general-workers  $!general-tasks-queued  $!general-tasks-completed
  $!timer-workers    $!timer-tasks-queued    $!timer-tasks-completed
  $!affinity-workers $!affinity-tasks-queued $!affinity-tasks-completed
>;

# Subroutines that are exported with :COLUMNS ----------------------------------
sub cpu() is raw is export(:COLUMNS) {
    my \rusage = nqp::getrusage;
    nqp::atpos_i(rusage,UTIME_SEC) * 1000000
      + nqp::atpos_i(rusage,UTIME_MSEC)
      + nqp::atpos_i(rusage,STIME_SEC) * 1000000
      + nqp::atpos_i(rusage,STIME_MSEC)
}

sub cpu-user() is raw is export(:COLUMNS) {
    my \rusage = nqp::getrusage;
    nqp::atpos_i(rusage,UTIME_SEC) * 1000000 + nqp::atpos_i(rusage,UTIME_MSEC)
}

sub cpu-sys() is raw is export(:COLUMNS) {
    my \rusage = nqp::getrusage;
    nqp::atpos_i(rusage,STIME_SEC) * 1000000 + nqp::atpos_i(rusage,STIME_MSEC)
}

sub max-rss() is raw is export(:COLUMNS) {
    nqp::bitshiftr_i(nqp::atpos_i(nqp::getrusage,MAX_RSS),$b2kb)
}

sub ix-rss() is raw is export(:COLUMNS) {
    nqp::bitshiftr_i(nqp::atpos_i(nqp::getrusage,IX_RSS),$b2kb)
}

sub id-rss() is raw is export(:COLUMNS) {
    nqp::bitshiftr_i(nqp::atpos_i(nqp::getrusage,ID_RSS),$b2kb)
}

sub is-rss() is raw is export(:COLUMNS) {
    nqp::bitshiftr_i(nqp::atpos_i(nqp::getrusage,IS_RSS),$b2kb)
}

sub min-flt() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,MIN_FLT)
}

sub maj-flt() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,MAJ_FLT)
}

sub nswap() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,NSWAP)
}

sub inblock() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,INBLOCK)
}

sub outblock() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,OUTBLOCK)
}

sub msgsnd() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,MSGSND)
}

sub msgrcv() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,MSGRCV)
}

sub nsignals() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,NSIGNALS)
}

sub nvcsw() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,NVCSW)
}

sub invcsw() is raw is export(:COLUMNS) {
    nqp::atpos_i(nqp::getrusage,INVCSW)
}

sub wallclock() is raw is export(:COLUMNS) {
    nqp::fromnum_I(1000000 * nqp::sub_n(nqp::time_n,$start),Int)
}

sub supervisor() is raw is export(:COLUMNS) {
    nqp::istrue(
      nqp::getattr(nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!supervisor')
    )
}

sub general-workers() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-workers'
      ))),
      nqp::elems($workers)
    )
}

sub general-tasks-queued() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $queue := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-queue'
      ))),
      nqp::elems($queue)
    )
}

sub general-tasks-completed() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!general-workers'
      ))),
      completed($workers)
    )
}

sub timer-workers() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-workers'
      ))),
      nqp::elems($workers)
    )
}

sub timer-tasks-queued() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $queue := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-queue'
      ))),
      nqp::elems($queue)
    )
}

sub timer-tasks-completed() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!timer-workers'
      ))),
      completed($workers)
    )
}

sub affinity-workers() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!affinity-workers'
      ))),
      nqp::elems($workers)
    )
}

sub affinity-tasks-queued() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!affinity-workers'
      ))),
      queued($workers)
    )
}

sub affinity-tasks-completed() is raw is export(:COLUMNS) {
    nqp::if(
      nqp::istrue((my $workers := nqp::getattr(
        nqp::decont($*SCHEDULER),ThreadPoolScheduler,'$!affinity-workers'
      ))),
      completed($workers)
    )
}

# Telemetry --------------------------------------------------------------------
class Telemetry {
    has Mu $!rusage;
    has num $!wallclock;
    has int $!supervisor;
    has int $!general-workers;
    has int $!general-tasks-queued;
    has int $!general-tasks-completed;
    has int $!timer-workers;
    has int $!timer-tasks-queued;
    has int $!timer-tasks-completed;
    has int $!affinity-workers;
    has int $!affinity-tasks-queued;
    has int $!affinity-tasks-completed;

    submethod BUILD() {
        $!rusage   := nqp::getrusage;
        $!wallclock = nqp::time_n;

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
            my int $elems = $!affinity-workers = nqp::elems(workers);
            my int $completed;
            my int $queued;
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::stmts(
                (my $w := nqp::atpos(workers,$i)),
                ($completed = nqp::add_i(
                  $completed,
                  nqp::getattr_i($w,$w.WHAT,'$!total')
                )),
                ($queued = nqp::add_i(
                  $queued,
                  nqp::elems(nqp::getattr($w,$w.WHAT,'$!queue'))
                ))
              )
            );
            $!affinity-tasks-queued    = $queued;
            $!affinity-tasks-completed = $completed;
        }

    }

    multi method cpu(Telemetry:U:) is raw { cpu }
    multi method cpu(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,UTIME_SEC) * 1000000
          + nqp::atpos_i($!rusage,UTIME_MSEC)
          + nqp::atpos_i($!rusage,STIME_SEC) * 1000000
          + nqp::atpos_i($!rusage,STIME_MSEC)
    }

    multi method cpu-user(Telemetry:U:) is raw { cpu-user }
    multi method cpu-user(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,UTIME_SEC) * 1000000
          + nqp::atpos_i($!rusage,UTIME_MSEC)
    }

    multi method cpu-sys(Telemetry:U:) is raw { cpu-sys }
    multi method cpu-sys(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,STIME_SEC) * 1000000
          + nqp::atpos_i($!rusage,STIME_MSEC)
    }

    multi method max-rss(Telemetry:U:) is raw { max-rss }
    multi method max-rss(Telemetry:D:) is raw {
        nqp::bitshiftr_i(nqp::atpos_i($!rusage,MAX_RSS),$b2kb)
    }

    multi method ix-rss(Telemetry:U:) is raw { ix-rss }
    multi method ix-rss(Telemetry:D:) is raw {
        nqp::bitshiftr_i(nqp::atpos_i($!rusage,IX_RSS),$b2kb)
    }

    multi method id-rss(Telemetry:U:) is raw {   id-rss }
    multi method id-rss(Telemetry:D:) is raw {
        nqp::bitshiftr_i(nqp::atpos_i($!rusage,ID_RSS),$b2kb)
    }

    multi method is-rss(Telemetry:U:) is raw { is-rss }
    multi method is-rss(Telemetry:D:) is raw {
        nqp::bitshiftr_i(nqp::atpos_i($!rusage,IS_RSS),$b2kb)
    }

    multi method min-flt(Telemetry:U:) is raw { min-flt }
    multi method min-flt(Telemetry:D:) is raw { nqp::atpos_i($!rusage,MIN_FLT) }

    multi method maj-flt(Telemetry:U:) is raw { maj-flt }
    multi method maj-flt(Telemetry:D:) is raw { nqp::atpos_i($!rusage,MAJ_FLT) }

    multi method nswap(Telemetry:U:) is raw { nswap }
    multi method nswap(Telemetry:D:) is raw { nqp::atpos_i($!rusage,NSWAP) }

    multi method inblock(Telemetry:U:) is raw { inblock }
    multi method inblock(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,INBLOCK)
    }

    multi method outblock(Telemetry:U:) is raw { outblock }
    multi method outblock(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,OUTBLOCK)
    }

    multi method msgsnd(Telemetry:U:) is raw { msgsnd }
    multi method msgsnd(Telemetry:D:) is raw { nqp::atpos_i($!rusage,MSGSND) }

    multi method msgrcv(Telemetry:U:) is raw { msgrcv }
    multi method msgrcv(Telemetry:D:) is raw { nqp::atpos_i($!rusage,MSGRCV) }

    multi method nsignals(Telemetry:U:) is raw { nsignals }
    multi method nsignals(Telemetry:D:) is raw {
        nqp::atpos_i($!rusage,NSIGNALS)
    }

    multi method nvcsw(Telemetry:U:) is raw { nvcsw }
    multi method nvcsw(Telemetry:D:) is raw { nqp::atpos_i($!rusage,NVCSW) }

    multi method invcsw(Telemetry:U:) is raw { invcsw }
    multi method invcsw(Telemetry:D:) is raw { nqp::atpos_i($!rusage,INVCSW) }

    multi method wallclock(Telemetry:U:) is raw { wallclock }
    multi method wallclock(Telemetry:D:) is raw {
        nqp::fromnum_I(1000000 * nqp::sub_n($!wallclock,$start),Int)
    }

    multi method supervisor(Telemetry:U:) is raw {   supervisor }
    multi method supervisor(Telemetry:D:) is raw { $!supervisor }

    multi method general-workers(Telemetry:U:) is raw {   general-workers }
    multi method general-workers(Telemetry:D:) is raw { $!general-workers }

    multi method general-tasks-queued(Telemetry:U:) is raw {
        general-tasks-queued
    }
    multi method general-tasks-queued(Telemetry:D:) is raw {
        $!general-tasks-queued
    }

    multi method general-tasks-completed(Telemetry:U:) is raw {
        general-tasks-completed
    }
    multi method general-tasks-completed(Telemetry:D:) is raw {
        $!general-tasks-completed
    }

    multi method timer-workers(Telemetry:U:) is raw {   timer-workers }
    multi method timer-workers(Telemetry:D:) is raw { $!timer-workers }

    multi method timer-tasks-queued(Telemetry:U:) is raw {
        timer-tasks-queued
    }
    multi method timer-tasks-queued(Telemetry:D:) is raw {
        $!timer-tasks-queued
    }

    multi method timer-tasks-completed(Telemetry:U:) is raw {
        timer-tasks-completed
    }
    multi method timer-tasks-completed(Telemetry:D:) is raw {
        $!timer-tasks-completed
    }

    multi method affinity-workers(Telemetry:U:) {   affinity-workers }
    multi method affinity-workers(Telemetry:D:) { $!affinity-workers }

    multi method affinity-tasks-queued(Telemetry:U:) is raw {
        affinity-tasks-queued
    }
    multi method affinity-tasks-queued(Telemetry:D:) is raw {
        $!affinity-tasks-queued
    }

    multi method affinity-tasks-completed(Telemetry:U:) is raw {
        affinity-tasks-completed
    }
    multi method affinity-tasks-completed(Telemetry:D:) is raw {
        $!affinity-tasks-completed
    }

    multi method Str(Telemetry:D:) {
        "$.cpu / $.wallclock"
    }
    multi method gist(Telemetry:D:) {
        "$.cpu / $.wallclock"
    }

    multi method AT-KEY(Telemetry:D: $key) { self."$key"() }
}

# Telemetry::Period ------------------------------------------------------------
class Telemetry::Period is Telemetry {

    # The external .new with slower named parameter interface
    multi method new(Telemetry::Period:
      int :$cpu-user,
      int :$cpu-sys,
      int :$max-rss,
      int :$ix-rss,
      int :$id-rss,
      int :$is-rss,
      int :$min-flt,
      int :$maj-flt,
      int :$nswap,
      int :$inblock,
      int :$outblock,
      int :$msgsnd,
      int :$msgrcv,
      int :$nsignals,
      int :$nvcsw,
      int :$invcsw,
      int :$wallclock,
      # non-special handling of other native integer nameds caught in %_
    ) {
        my $period := nqp::create(self);

        # set all fields in the rusage struct
        my \rusage = nqp::getrusage; # make sure we get the same thing
        nqp::bindpos_i(rusage, UTIME_SEC,$cpu-user div 1000000);
        nqp::bindpos_i(rusage,UTIME_MSEC,$cpu-user  %  1000000);
        nqp::bindpos_i(rusage, STIME_SEC,$cpu-sys div 1000000);
        nqp::bindpos_i(rusage,STIME_MSEC,$cpu-sys  %  1000000);
        for @rusage_names.kv -> int $i, $name {
            nqp::bindpos_i($period,$i,%_{$name.substr(2)})
              if $name && %_.EXISTS-KEY($name.substr(2))
        }

        # create object with special cases
        nqp::bindattr($period,Telemetry,'$!rusage',rusage);
        nqp::bindattr_n($period,Telemetry,'$!wallclock',
          nqp::add_n($start,$wallclock / 1000000)
        );

        # diff all attribute_i attributes
        nqp::bindattr_i($period,Telemetry,$_,%_{.substr(2)})
          if %_{.substr(2)}:exists for @scheduler_names;

        $period
    }

    # For roundtripping
    multi method perl(Telemetry::Period:D:) {
        my \rusage := nqp::getattr(self,Telemetry,'$!rusage');

        "Telemetry::Period.new(:cpu-user($.cpu-user),:cpu-sys($.cpu-sys),"
          ~ @rusage_names.kv.map( -> int $i, $name {
              ":$name\({nqp::atpos_i(rusage,$i)})" if $name
            }).join(",")
          ~ @scheduler_names.map({
              ":$_.substr(2)\({nqp::getattr_i(self,Telemetry,$_)})"
            }).join(",")
    }

    my int $cores = Kernel.cpu-cores;
    method cpus() {
        (my int $wallclock = self.wallclock)
          ?? self.cpu / $wallclock
          !! $cores
    }

    my $factor = 100 / $cores;
    method utilization() { $factor * self.cpus }
}

# Creating Telemetry::Period objects -------------------------------------------
multi sub infix:<->(Telemetry:U \a, Telemetry:U \b) is export {
    nqp::create(Telemetry::Period)
}
multi sub infix:<->(Telemetry:D \a, Telemetry:U \b) is export { a - b.new }
multi sub infix:<->(Telemetry:U \a, Telemetry:D \b) is export { a.new - b }
multi sub infix:<->(Telemetry:D \a, Telemetry:D \b) is export {
    my $a := nqp::decont(a);
    my $b := nqp::decont(b);
    
    # create diff of rusage structs
    my Mu \rusage-a = nqp::decont(nqp::getattr($a,Telemetry,'$!rusage'));
    my Mu \rusage-b = nqp::decont(nqp::getattr($b,Telemetry,'$!rusage'));
    my Mu \rusage   = nqp::clone(rusage-a);  # make sure correct type
    my int $i = -1;
    nqp::while(
      ++$i < RUSAGE_ELEMS,
      nqp::bindpos_i(rusage,$i,
        nqp::sub_i(nqp::atpos_i(rusage-a,$i),nqp::atpos_i(rusage-b,$i))
      )
    );

    # create object with special cases
    my $period := nqp::create(Telemetry::Period);
    nqp::bindattr($period,Telemetry,'$!rusage',rusage);
    nqp::bindattr_n($period,Telemetry,'$!wallclock',
      nqp::add_n($start,nqp::sub_n(
        nqp::getattr_n($a,Telemetry,'$!wallclock'),
        nqp::getattr_n($b,Telemetry,'$!wallclock')
      ))
    );

    # diff all attribute_i attributes
    nqp::bindattr_i($period,Telemetry,$_,nqp::sub_i(
      nqp::getattr_i($a,Telemetry,$_),
      nqp::getattr_i($b,Telemetry,$_)
    )) for @scheduler_names;

    $period
}

# Subroutines that are always exported -----------------------------------------

# Making a Telemetry object procedurally 
my @snaps;
proto sub snap(|) is export {*}
multi sub snap(--> Nil)    { @snaps.push(Telemetry.new) }
multi sub snap(@s --> Nil) { @s.push(Telemetry.new) }

# Starting the snapper / changing the period size
my int $snapper-running;
my $snapper-wait;
sub snapper($sleep = 0.1 --> Nil) is export {
    $snapper-wait = $sleep;
    unless $snapper-running {
        snap;
        Thread.start(:app_lifetime, :name<Snapper>, {
            loop { sleep $snapper-wait; snap }
        });
        $snapper-running = 1
    }
}

# Telemetry::Period objects from a list of Telemetry objects
proto sub periods(|) is export {*}
multi sub periods() {
    my @s = @snaps;
    @snaps = ();
    @s.push(Telemetry.new) if @s == 1;
    periods(@s)
}
multi sub periods(@s) { (1..^@s).map: { @s[$_] - @s[$_ - 1] } }

# Telemetry reporting features -------------------------------------------------
proto sub report(|) is export {*}
multi sub report(:@columns, :$legend, :$header-repeat = 32) {
    my $s := nqp::clone(nqp::getattr(@snaps,List,'$!reified'));
    nqp::setelems(nqp::getattr(@snaps,List,'$!reified'),0);
    nqp::push($s,Telemetry.new) if nqp::elems($s) == 1;
    report(
      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$s),
      :@columns,
      :$legend,
      :$header-repeat,
    );
}

# Convert to spaces if numeric value is 0
sub hide0(\value, int $size = 3) {
    value ?? value.fmt("%{$size}d") !! nqp::x(" ",$size)
}

# Set up how to handle report generation (in alphabetical order)
my %format =
  affinity-tasks-completed =>
    [ "     atc", { hide0(.affinity-tasks-completed,8) },
      "The number of tasks completed in affinity threads"],
  affinity-tasks-queued =>
    [ "atq", { hide0(.affinity-tasks-queued) },
      "The number of tasks queued for execution in affinity threads"],
  affinity-workers =>
    [     " aw", { hide0(.affinity-workers) },
      "The number of affinity threads"],
  cpu =>
    ["     cpu", { .cpu.fmt('%8d') },
      "The amount of CPU used (in microseconds)"],
  cpu-user =>
    ["cpu-user", { .cpu.fmt('%8d') },
      "The amount of CPU used in user code (in microseconds)"],
  cpu-sys =>
    [" cpu-sys", { .cpu.fmt('%8d') },
      "The amount of CPU used in system overhead (in microseconds)"],
  general-workers =>
    [     " gw", { hide0(.general-workers) },
      "The number of general worker threads"],
  general-tasks-queued =>
    [     "gtq", { hide0(.general-tasks-queued) },
      "The number of tasks queued for execution in general worker threads"],
  general-tasks-completed =>
    [ "     gtc", { hide0(.general-tasks-completed,8) },
      "The number of tasks completed in general worker threads"],
  id-rss =>
    ["  id-rss", { hide0(.id-rss,8) },
      "Integral unshared data size (in Kbytes)"],
  inblock =>
    ["inb", { hide0(.inblock) },
      "Number of block input operations"],
  invcsw =>
    ["     ics", { hide0(.invcsw,8) },
      "Number of involuntary context switches"],
  is-rss =>
    ["  is-rss", { hide0(.id-rss,8) },
      "Integral unshared stack size (in Kbytes)"],
  ix-rss =>
    ["  ix-rss", { hide0(.ix-rss,8) },
      "Integral shared text memory size (in Kbytes)"],
  maj-flt =>
    ["aft", { hide0(.maj-flt,3) },
      "Number of page reclaims (ru_majflt)"],
  max-rss =>
    [" max-rss", { hide0(.max-rss,8) },
      "Maximum resident set size (in Kbytes)"],
  min-flt =>
    ["ift", { hide0(.min-flt) },
      "Number of page reclaims (ru_minflt)"],
  msgrcv =>
    ["mrc", { hide0(.msgrcv) },
      "Number of messages received"],
  msgsnd =>
    ["msd", { hide0(.msgsnd) },
      "Number of messages sent"],
  nsignals =>
    ["ngs", { hide0(.nsignals) },
      "Number of signals received"],
  nswap =>
    ["nsw", { hide0(.nswap) },
      "Number of swaps"],
  nvcsw =>
    [" vcs", { hide0(.nvcsw,4) },
      "Number of voluntary context switches"],
  outblock =>
    ["oub", { hide0(.outblock) },
      "Number of block output operations"],
  supervisor =>
    [       "s", { hide0(.supervisor,1) },
      "The number of supervisors"],
  timer-workers =>
    [     " tw", { hide0(.timer-workers) },
      "The number of timer threads"],
  timer-tasks-queued =>
    [     "ttq", { hide0(.timer-tasks-queued) },
      "The number of tasks queued for execution in timer threads"],
  timer-tasks-completed =>
    [ "     ttc", { hide0(.timer-tasks-completed,8) },
      "The number of tasks completed in timer threads"],
  utilization =>
    [  " util%", { .utilization.fmt('%6.2f') },
      "Percentage of CPU utilization (0..100%)"],
  wallclock =>
    ["wallclock", { hide0(.wallclock,9) },
      "Number of microseconds elapsed"],
;

# Set footer and make sure we can also use the header key as an indicator
for %format.values -> \v {
    v[3] = '-' x v[0].chars;
    %format{v[0].trim} = v;
}

multi sub report(
  @s,
  :@columns is copy,
  :$legend,
  :$header-repeat = 32,
) {

    unless @columns {
        if %*ENV<RAKUDO_REPORT_COLUMNS> -> $rrc {
            @columns = $rrc.comb( /<[\w-]>+/ );
        }
        else {
            @columns = <wallclock util% max-rss gw gtc tw ttc aw atc>;
        }
    }

    my $total = @s[*-1] - @s[0];
    my $text := nqp::list_s(qq:to/HEADER/.chomp);
Telemetry Report of Process #$*PID ({Instant.from-posix(nqp::time_i).DateTime})
Number of Snapshots: {+@s}
Initial Size:    { @s[0].max-rss.fmt('%9d') } Kbytes
Total Time:      { ($total.wallclock / 1000000).fmt('%9.2f') } seconds
Total CPU Usage: { ($total.cpu / 1000000).fmt('%9.2f') } seconds
HEADER

    sub push-period($period) {
        nqp::push_s($text,
          %format{@columns}>>.[1]>>.($period).join(' ').trim-trailing);
    }

    my $header = "\n%format{@columns}>>.[0].join(' ')";
    nqp::push_s($text,$header) unless $header-repeat;

    for periods(@s).kv -> $index, $period {
        nqp::push_s($text,$header)
          if $header-repeat && $index %% $header-repeat;
        push-period($period)
    }

    nqp::push_s($text,%format{@columns}>>.[3].join(' '));

    push-period($total);

    if $legend {
        nqp::push_s($text,'');
        nqp::push_s($text,'Legend:');
        for %format{@columns} -> $col {
            nqp::push_s($text," $col[0].trim-leading.fmt('%9s')  $col[2]");
        }
    }

    nqp::join("\n",$text)
}

# The special T<foo bar> functionality -----------------------------------------

sub T () is export { Telemetry.new }

# Make sure we tell the world if we're implicitely told to do so ---------------
END { if @snaps { snap; note report(:legend) } }

# vim: ft=perl6 expandtab sw=4
