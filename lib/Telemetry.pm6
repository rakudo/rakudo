# Provide an API for keeping track of a lot of system lifesigns

use nqp;

# Telemetry::Instrument role for building instruments --------------------------
role Telemetry::Instrument does Associative is export {
    has Mu $!data;
    method data() is raw { $!data }

    multi method new(::?CLASS:) {
        my $self := nqp::create(self);
        nqp::bindattr($self,self,'$!data',$self!snap);
        $self
    }
    multi method new(::?CLASS: Mu $data) {  # provided for .perl roundtripping
        my $self := nqp::create(self);
        nqp::bindattr($self,::?CLASS,'$!data',nqp::decont($data));
        $self
    }
    multi method new(::?CLASS: *@data) {    # provided for .perl roundtripping
        my $self := nqp::create(self);
        nqp::bindattr($self,self,'$!data',my $data := nqp::list_i);
        nqp::push_i($data,$_) for @data;
        $self
    }

    multi method perl(::?CLASS:D:) {
        my $text := nqp::list_s;
        my int $elems = nqp::elems($!data);
        my int $i = -1;
        nqp::while(
          ++$i < $elems,
          nqp::push_s($text,nqp::atpos_i($!data,$i))
        );

        self.^name ~ '.new(' ~ nqp::join(',',$text) ~ ')'
    }

    method !snap() is raw { ... }
    method formats() { ... }
    method columns() { ... }
    method AT-KEY($key) { ... }
    method EXISTS-KEY($key) { ... }
}

# Telemetry data from wallclock and nqp::getrusage -----------------------------
class Telemetry::Instrument::Usage does Telemetry::Instrument {

    # Helper stuff
    my int $start = nqp::fromnum_I(Rakudo::Internals.INITTIME * 1000000,Int);
    my int $cores = Kernel.cpu-cores;
    my $utilize   = 100 / $cores;
    my int $b2kb =
      nqp::atkey(nqp::backendconfig,'osname') eq 'darwin' ?? 10 !! 0;

    # Constants indexing into the data array
    my constant UTIME_SEC  =  0;
    my constant UTIME_MSEC =  1;
    my constant STIME_SEC  =  2;
    my constant STIME_MSEC =  3;
    my constant MAX_RSS    =  4;
    my constant IX_RSS     =  5;
    my constant ID_RSS     =  6;
    my constant IS_RSS     =  8;
    my constant MIN_FLT    =  9;
    my constant MAJ_FLT    = 10;
    my constant NSWAP      = 11;
    my constant INBLOCK    = 12;
    my constant OUTBLOCK   = 13;
    my constant MSGSND     = 14;
    my constant MSGRCV     = 14;
    my constant NSIGNALS   = 15;
    my constant NVCSW      = 16;
    my constant INVCSW     = 17;
    my constant WALLCLOCK  = 18;   # not actually part of nqp::getrusage

    # Initialize the dispatch hash using HLL features, as we only need to
    # do this on module load time.  First handle the usable names of
    # attributes that are part of getrusage struct.
    my %dispatch = << "" "" "" "" # first 4 are special
      max-rss  ix-rss id-rss is-rss   min-flt maj-flt nswap inblock
      outblock msgsnd msgrcv nsignals nvcsw   invcsw  wallclock
    >>.kv.map: -> int $index, $name {
        if $name {
            $name => $name.ends-with('rss') && $b2kb
              ?? -> Mu \data {
                       nqp::bitshiftr_i(nqp::atpos_i(data,$index),$b2kb)
                    }
              !! -> Mu \data {
                       nqp::atpos_i(data,$index)
                    }
        }
    }

    # Allow for low-level dispatch hash access for speed
    my $dispatch := nqp::getattr(%dispatch,Map,'$!storage');

    # Add the special cases to the dispatch
    %dispatch<cpu> = -> Mu \data {
        nqp::atpos_i(data,UTIME_SEC) * 1000000
          + nqp::atpos_i(data,UTIME_MSEC)
          + nqp::atpos_i(data,STIME_SEC) * 1000000
          + nqp::atpos_i(data,STIME_MSEC)
    }
    %dispatch<cpu-user> = -> Mu \data {
        nqp::atpos_i(data,UTIME_SEC) * 1000000
          + nqp::atpos_i(data,UTIME_MSEC)
    }
    %dispatch<cpu-sys> = -> Mu \data {
        nqp::atpos_i(data,STIME_SEC) * 1000000
          + nqp::atpos_i(data,STIME_MSEC)
    }
    %dispatch<cpus> = -> Mu \data {
        (my int $wallclock = nqp::atkey($dispatch,'wallclock')(data))
          ?? nqp::atkey($dispatch,'cpu')(data) / $wallclock
          !! $cores
    }
    %dispatch<util%> = -> Mu \data {
        $utilize * nqp::atkey($dispatch,'cpus')(data)
    }

    method formats() is raw {
           << cpu 8d
          'The total amount of CPU used (in microseconds)'
        >>,<< cpu-user 8d
          'The amount of CPU used in user code (in microseconds)'
        >>,<< cpu-sys 8d
          'The amount of CPU used in system overhead (in microseconds)'
        >>,<< id-rss 8d
          'Integral unshared data size (in Kbytes)'
        >>,<< inb 3d
          'Number of block input operations'
        >>,<< invcsw 8d
          'Number of involuntary context switches'
        >>,<< is-rss 8d
          'Integral unshared stack size (in Kbytes)'
        >>,<< ix-rss 8d
          'Integral shared text memory size (in Kbytes)'
        >>,<< aft 3d
          'Number of page reclaims (ru_majflt)'
        >>,<< max-rss 8d
          'Maximum resident set size (in Kbytes)'
        >>,<< ift 3d
          'Number of page reclaims (ru_minflt)'
        >>,<< mrc 3d
          'Number of messages received'
        >>,<< msd 3d
          'Number of messages sent'
        >>,<< ngs 3d
          'Number of signals received'
        >>,<< nsw 3d
          'Number of swaps'
        >>,<< vcs 4d
          'Number of voluntary context switches'
        >>,<< oub 3d
          'Number of block output operations'
        >>,<< util% 6.2f
          'Percentage of CPU utilization (0..100%)'
        >>,<< wallclock 9d
          'Number of microseconds elapsed'
        >>
    }

    method columns() { < wallclock util% max-rss > }

    method AT-KEY(Str:D $key) {
        nqp::ifnull(
          nqp::atkey($dispatch,$key),
          -> Mu \data { Nil }
        )($!data)
    }

    method EXISTS-KEY(Str:D $key) { nqp::existskey($dispatch,$key) }

    method !snap() is raw {
        nqp::stmts(
          nqp::bindpos_i(
            (my $data := nqp::getrusage),
            WALLCLOCK,
            nqp::sub_i(nqp::fromnum_I(nqp::time_n() * 1000000,Int),$start)
          ),
          $data
        )
    }
}

# Telemetry data from the ThreadPoolScheduler ----------------------------------
class Telemetry::Instrument::ThreadPool does Telemetry::Instrument {

    # Constants indexing into the data array
    my constant SUPERVISOR =  0;
    my constant GW         =  1;
    my constant GTQ        =  2;
    my constant GTC        =  3;
    my constant TW         =  4;
    my constant TTQ        =  5;
    my constant TTC        =  6;
    my constant AW         =  7;
    my constant ATQ        =  8;
    my constant ATC        =  9;
    my constant COLUMNS    = 10;

    # Initialize the dispatch hash using HLL features, as we only need to
    # do this on module load time.  First handle the usable names of
    # attributes that are part of getrusage struct.
    my %dispatch = <<
      s gw gtq gtc tw ttq ttc aw atq atc
    >>.kv.map: -> int $index, $name {
        $name => -> Mu \data { nqp::atpos_i(data,$index) }
    }

    # Allow for low-level dispatch hash access for speed
    my $dispatch := nqp::getattr(%dispatch,Map,'$!storage');

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

    method formats() is raw {
           << atc 8d
           'The number of tasks completed in affinity threads'
        >>,<< atq 3d
           'The number of tasks queued for execution in affinity threads'
        >>,<< aw 3d
           'The number of affinity threads'
        >>,<< gtc 8d
           'The number of tasks completed in general worker threads'
        >>,<< gtq 3d
           'The number of tasks queued for execution in general worker threads'
        >>,<< gw 3d
           'The number of general worker threads'
        >>,<< s 1d
           'The number of supervisors'
        >>,<< ttc 8d
           'The number of tasks completed in timer threads'
        >>,<< ttq 3d
           'The number of tasks queued for execution in timer threads'
        >>,<< tw 3d
           'The number of timer threads'
        >>
    }

    method columns() { < gw gtc tw ttc aw atc > }

    method AT-KEY(Str:D $key) {
        nqp::ifnull(
          nqp::atkey($dispatch,$key),
          -> Mu \data { Nil }
        )($!data)
    }

    method EXISTS-KEY(Str:D $key) { nqp::existskey($dispatch,$key) }

    method !snap() is raw {
        my $data := nqp::setelems(nqp::list_i,COLUMNS);

        if $*SCHEDULER -> \scheduler {
            my $sched := nqp::decont(scheduler);

            nqp::bindpos_i($data,SUPERVISOR,1)
              if nqp::getattr($sched,ThreadPoolScheduler,'$!supervisor');

            if nqp::getattr($sched,ThreadPoolScheduler,'$!general-workers')
              -> \workers {
                nqp::bindpos_i($data,GW,nqp::elems(workers));
                if nqp::getattr($sched,ThreadPoolScheduler,'$!general-queue')
                  -> \queue {
                    nqp::bindpos_i($data,GTQ,nqp::elems(queue));
                }
                nqp::bindpos_i($data,GTC,completed(workers));
            }

            if nqp::getattr($sched,ThreadPoolScheduler,'$!timer-workers')
              -> \workers {
                nqp::bindpos_i($data,TW,nqp::elems(workers));
                if nqp::getattr($sched,ThreadPoolScheduler,'$!timer-queue')
                  -> \queue {
                    nqp::bindpos_i($data,TTQ,mnqp::elems(queue));
                }
                nqp::bindpos_i($data,TTC,completed(workers));
            }

            if nqp::getattr($sched,ThreadPoolScheduler,'$!affinity-workers')
              -> \workers {
                my int $elems = nqp::bindpos_i($data,AW,nqp::elems(workers));
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
                nqp::bindpos_i($data,ATQ,$queued);
                nqp::bindpos_i($data,ATC,$completed);
            }
        }

        # the final thing
        $data
    }
}

# Telemetry::Sampler -----------------------------------------------------------
class Telemetry::Sampler {
    has $!instruments;
    has $!dispatcher;
    has $!formats;

    multi method new(Telemetry::Sampler:) { self.new([]) }
    multi method new(Telemetry::Sampler: Mu \instrument) {
        self.new(List.new(instrument))
    }
    multi method new(Telemetry::Sampler: @spec) {
        my $self := nqp::create(self);
        nqp::bindattr($self,self,'$!instruments',
          my $instruments := nqp::create(IterationBuffer));
        nqp::bindattr($self,self,'$!dispatcher',
          my $dispatcher := nqp::create(Rakudo::Internals::IterationSet));
        nqp::bindattr($self,self,'$!formats',
          my $formats := nqp::create(Rakudo::Internals::IterationSet));

        # helper sub for handling instruments specified with a Str
        sub Str-instrument($name) {
            (my $class := nqp::decont(Telemetry::Instrument::{$name})) =:= Any
              ?? die "Could not find Telemetry::Instrument::$name class"
              !! $class
        }

        sub set-up-instrument($class --> Nil) {
            my int $index = nqp::elems($instruments);
            $instruments.push($class);

            my constant KEY    = 0;
            my constant FORMAT = 1;
            my constant LEGEND = 2;

            for $class.formats -> @info {
                my str $key = @info[KEY];
                nqp::bindkey($dispatcher,$key, -> Mu \samples {
                  nqp::atpos(samples,$index).AT-KEY($key)
                });
                nqp::bindkey($formats,$key,@info);
            }
        }

        # handle instrument specification
        if @spec {
            for @spec {
                when Str {
                    set-up-instrument(Str-instrument($_));
                }
                default {
                    set-up-instrument($_);
                }
            }
        }

        # none specified, but we do have a default in the environment
        elsif %*ENV<RAKUDO_TELEMETRY_INSTRUMENTS> -> $rri {
            set-up-instrument(Str-instrument($_)) for $rri.comb( /<[\w-]>+/ );
        }

        # no instruments to be found anywhere, use the default default
        else {
            set-up-instrument($_) for 
              Telemetry::Instrument::Usage,
              Telemetry::Instrument::ThreadPool,
            ;
        }

        $self
    }

    multi method perl(Telemetry::Sampler:D:) {
        self.^name
          ~ '.new('
          ~ self.instruments.map(*.^name).join(",")
          ~ ')'
    }

    method instruments(Telemetry::Sampler:D:) {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!instruments)
    }
    method formats(Telemetry::Sampler:D:) {
        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$!formats)
    }
}

# Make sure we alwas have a Sampler
without $*SAMPLER {
    PROCESS::<$SAMPLER> := Telemetry::Sampler.new;
}

# Telemetry --------------------------------------------------------------------
class Telemetry does Associative {
    has $!sampler;
    has $!samples;

    method new() {
        my $self := nqp::create(self);
        nqp::bindattr($self,self,'$!sampler',
          my $sampler := nqp::decont($*SAMPLER));
        my $instruments :=
          nqp::getattr($sampler,Telemetry::Sampler,'$!instruments');
        my int $elems = nqp::elems($instruments);
        nqp::bindattr($self,self,'$!samples',
          my $samples := nqp::setelems(nqp::create(IterationBuffer),$elems));

        my int $i = -1;
        nqp::while(
          ++$i < $elems,
          nqp::bindpos($samples,$i,nqp::atpos($instruments,$i).new)
        );

        $self
    }

    method sampler() { $!sampler }

    method samples() {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!samples)
    }

    method AT-KEY($key) is raw {
        nqp::ifnull(
          nqp::atkey(
            nqp::getattr($!sampler,Telemetry::Sampler,'$!dispatcher'),
            $key
          ),
          -> Mu \samples { Nil }
        )($!samples)
    }

    method FALLBACK(Telemetry:D: $method) is raw {
        self.AT-KEY($method)
          // X::Method::NotFound.new(:$method,:typename(self.^name)).throw
    }
}

# Telemetry::Period ------------------------------------------------------------
class Telemetry::Period is Telemetry {
    # Same as Telemetry, but contains differences instead of absolute values
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
    my $period := nqp::create(Telemetry::Period);
    nqp::bindattr($period,Telemetry,'$!sampler',
      nqp::getattr($a,Telemetry,'$!sampler'));

    my \samples-a := nqp::getattr($a,Telemetry,'$!samples');
    my \samples-b := nqp::getattr($b,Telemetry,'$!samples');
    my int $elems = nqp::elems(samples-a);
    die "Different number of samples" if $elems != nqp::elems(samples-b);

    # create diff of rusage structs
    sub diff($a, $b) is raw {
        my Mu \data-a = nqp::decont($a.data);
        my Mu \data-b = nqp::decont($b.data);
        my Mu \data   = nqp::clone(data-a);  # make sure correct type

        my int $i = -1;
        my int $elems = nqp::elems(data);
        nqp::while(
          ++$i < $elems,
          nqp::bindpos_i(data,$i,
            nqp::sub_i(nqp::atpos_i(data-a,$i),nqp::atpos_i(data-b,$i))
          )
        );

        $a.new(data)
    }

    nqp::bindattr($period,Telemetry,'$!samples',
      my \samples := nqp::setelems(nqp::create(IterationBuffer),$elems));
    my int $i = -1;
    nqp::while(
      ++$i < $elems,
      nqp::bindpos(samples,$i,diff(
        nqp::atpos(samples-a,$i),
        nqp::atpos(samples-b,$i)
      ))
    );

    $period
}

# Making a Telemetry object procedurally ---------------------------------------
my @snaps;
proto sub snap(|) is export {*}
multi sub snap(--> Nil)    { @snaps.push(Telemetry.new) }
multi sub snap(@s --> Nil) { @s.push(Telemetry.new) }

# Starting the snapper / changing the period size
my int $snapper-running;
my $snapper-wait;
sub snapper($sleep = 0.1, :$stop, :$reset --> Nil) is export {

    $snapper-wait = $sleep;
    nqp::bindattr(@snaps,List,'$!reified',nqp::list) if $reset;

    if $snapper-running {
        $snapper-running = 0 if $stop;
    }
    elsif !$stop {
        $snapper-running = 1;
        Thread.start(:app_lifetime, :name<Snapper>, {
            snap;
            while $snapper-running {
                sleep $snapper-wait;
                snap if $snapper-running;
            }
        });
    }
}

# Telemetry::Period objects from a list of Telemetry objects -------------------
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
multi sub report(:@columns, :$legend, :$header-repeat, :$csv, :@format) {

    # race condition, but should be safe enough because installing new list
    # and all access is done using HLL ops, so those will either see the old
    # or the new nqp::list, and thus push to either the old or the new.
    my $s := nqp::getattr(@snaps,List,'$!reified');
    nqp::bindattr(@snaps,List,'$!reified',nqp::list);

    report(
      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$s),
      :@columns,
      :$legend,
      :$header-repeat,
      :$csv,
      :@format,
    );
}

# some constants for the %format list
constant NAME    = 0; # short name
constant FORMAT  = 1; # format (without % prefixed)
constant LEGEND  = 2; # legend
constant HEADER  = 3; # generated: column header
constant FOOTER  = 4; # generated: column footer
constant DISPLAY = 5; # generated: code to execute to display

sub prepare-format(@raw, %format --> Nil) is raw {

    for @raw -> @info is copy {
        my str $name   = @info[NAME];
        my str $format = @info[FORMAT];
        my int $width  = $format; # natives have p5 semantics
        my str $empty  = nqp::x(" ",$width);

        @info[HEADER]  = $name.fmt("%{$width}s");
        @info[FOOTER]  = nqp::x("-",$width);
        @info[DISPLAY] = -> \value { value ?? value.fmt("%$format") !! $empty }

        %format{$name} = @info;
    }
}

multi sub report(
  @s,
  :@columns       is copy,
  :$header-repeat is copy,
  :$legend        is copy,
  :$csv           is copy,
  :@format,
) {

    # get the sampler that was used
    my $sampler := @s[0].sampler;

    # determine columns to be displayed
    unless @columns {
        if %*ENV<RAKUDO_REPORT_COLUMNS> -> $rrc {
            @columns = $rrc.comb( /<[\w-]>+/ );
        }
        else {
            @columns.append(.columns) for $sampler.instruments;
        }
    }

    # set header repeat flag
    without $header-repeat {
        $header-repeat = $_.Int with %*ENV<RAKUDO_REPORT_HEADER_REPEAT> // 32;
    }

    # set legend flag
    without $legend {
        $legend = $_.Int with %*ENV<RAKUDO_REPORT_LEGEND> // 1;
    }

    # set csv flag
    without $csv {
        $csv = $_.Int with %*ENV<RAKUDO_REPORT_CSV> // 0;
    }

    # get / calculate the format info we need
    my %format;
    if @format {
        prepare-format(@format, %format)
    }
    else {
        prepare-format(.formats, %format) for @s[0].sampler.instruments;
    }

    # some initializations
    my $text   := nqp::list_s;
    my @periods = periods(@s);

    # only want CSV ready output
    if $csv {
        my @formats = %format{@columns};
        nqp::push_s($text,%format{@columns}>>.[NAME].join(' '));
        for @periods -> $period {
            nqp::push_s($text,
              @formats.map( -> @info { $period{@info[NAME]} }).join(' ')
            )
        }
    }

    # standard text output
    else {
        my $first = @s[0];
        my $last  = @s[*-1];
        my $total = $last - $first;

        # remove the columns that don't have any values
        @columns = @columns.grep: -> $column {
            @periods.first: { .{%format{$column}[NAME]} }
        };
        my $header  = "\n%format{@columns}>>.[HEADER].join(' ')";
        my @formats = %format{@columns};

        nqp::push_s($text,qq:to/HEADER/.chomp);
Telemetry Report of Process #$*PID ({Instant.from-posix(nqp::time_i).DateTime})
Number of Snapshots: {+@s}
HEADER

        # give the supervisor blurb if we can
        if $sampler.instruments.grep( Telemetry::Instrument::ThreadPool ) {
            if $first<s> {
                nqp::push_s($text,"Supervisor thread ran for the whole time");
            }
            elsif !$last<s> {
                nqp::push_s($text,"No supervisor thread has been running");
            }
            else {
                my $started = @s.first: *.<s>;
                nqp::push_s($text,"Supervisor thread ran for {
                  (100 * ($last<wallclock> - $started<wallclock>)
                    / $total<wallclock>).fmt("%5.2f")
                }% of the time");
            }
        }

        # add general performance blurb if we can
        if $sampler.instruments.grep( Telemetry::Instrument::Usage ) {
            nqp::push_s($text,qq:to/HEADER/.chomp);
Initial Size:    { @s[0]<max-rss>.fmt('%9d') } Kbytes
Total Time:      { ($total<wallclock> / 1000000).fmt('%9.2f') } seconds
Total CPU Usage: { ($total<cpu> / 1000000).fmt('%9.2f') } seconds
HEADER
        }

        sub push-period($period --> Nil) {
            nqp::push_s($text,
              @formats.map( -> @info {
                  @info[DISPLAY]($period{@info[NAME]})
              }).join(' ').trim-trailing
            )
        }

        nqp::push_s($text,$header) unless $header-repeat;

        for @periods.kv -> $index, $period {
            nqp::push_s($text,$header)
              if $header-repeat && $index %% $header-repeat;
            push-period($period)
        }

        nqp::push_s($text,%format{@columns}>>.[FOOTER].join(' '));

        push-period($total);

        if $legend {
            nqp::push_s($text,'');
            nqp::push_s($text,'Legend:');
            for %format{@columns} -> $col {
                nqp::push_s($text,"$col[NAME].fmt("%9s")  $col[LEGEND]");
            }
        }
    }

    nqp::join("\n",$text)
}

# Allow for safe CTRL-c exit, always giving a report ---------------------------
my int $has-safe-ctrl-c;
sub safe-ctrl-c(--> Nil) is export {
    unless $has-safe-ctrl-c {
        signal(SIGINT).tap: &exit;
        $has-safe-ctrl-c = 1;
    }
}

# The special T<foo bar> functionality -----------------------------------------

sub T () is export { Telemetry.new }

# Provide limited export capability --------------------------------------------

sub EXPORT(*@args) {
    (EXPORT::DEFAULT::{ @args.map: '&' ~ * }:p).Map
}

# Make sure we tell the world if we're implicitely told to do so ---------------
END {
    $snapper-running = 0;  # stop any snapper
    if @snaps {
        snap;
        note report;
    }
}

# vim: ft=perl6 expandtab sw=4
