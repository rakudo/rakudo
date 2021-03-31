# Provide an API for keeping track of a lot of system lifesigns

use nqp;

use Perl6::Compiler:from<NQP>;

# the place where the default snaps are stored
my $snaps := nqp::create(IterationBuffer);

# Role for building instruments --------------------------
role Telemetry::Instrument {

    # Should return instantiated snap object
    method snap() is raw { ... } # Typically just Snap.new

    # Should return a list of lists with:
    #  [0] name of the column, also used in headers and legends
    #  [1] printf format of the column, *without* '%' prefix
    #  [2] one line explanation of the column to be used in legend
    method formats() { ... }

    # Should a return a list of column names to be used by default
    method default-columns() { ... }

    # Returns sorted list of all columns names
    method columns() { self.formats.map( *[0] ).sort }
}

# Role for creating an instrument snap -------------
role Telemetry::Instrument::Snap does Associative {
    has Mu $!data;
    method data() is raw { $!data }

    multi method new(::?CLASS:) {
        nqp::p6bindattrinvres(nqp::create(self),self,'$!data',self!snap)
    }
    multi method new(::?CLASS:D: Mu \data) { # needed for creating a difference
        nqp::p6bindattrinvres(
          nqp::clone(self),::?CLASS,'$!data',nqp::decont(data))
    }
    multi method new(::?CLASS: *@data) {     # provided for .raku roundtripping
        my $data := nqp::list_i;
        nqp::push_i($data,$_) for @data;
        nqp::p6bindattrinvres(nqp::create(self),self,'$!data',$data)
    }

    multi method raku(::?CLASS:D:) {
        my $text := nqp::list_s;
        my int $elems = nqp::elems($!data);
        my int $i = -1;
        nqp::while(
          ++$i < $elems,
          nqp::push_s($text,nqp::atpos_i($!data,$i))
        );

        self.^name ~ '.new(' ~ nqp::join(',',$text) ~ ')'
    }

    # Should return a native-int like list with a sample
    method !snap() is raw { ... }

    # Needed for associative access: given a column name, return the value
    method AT-KEY($column) { ... }

    # Needed for associative access: given a column name, return whether exists
    method EXISTS-KEY($column) { ... }
}

# Telemetry data from wallclock and nqp::getrusage -----------------------------
class Telemetry::Instrument::Usage does Telemetry::Instrument {

    method formats() is raw {
           << cpu 8d
          'The total amount of CPU used (in microseconds)'
        >>,<< cpu-sys 8d
          'The amount of CPU used in system overhead (in microseconds)'
        >>,<< cpu-user 8d
          'The amount of CPU used in user code (in microseconds)'
        >>,<< cpus 5.1f
          "The number of CPU's that were busy on average"
        >>,<< id-rss 8d
          'Integral unshared data size (in Kbytes)'
        >>,<< inb 4d
          'Number of block input operations'
        >>,<< invcsw 8d
          'Number of involuntary context switches'
        >>,<< is-rss 8d
          'Integral unshared stack size (in Kbytes)'
        >>,<< ix-rss 8d
          'Integral shared text memory size (in Kbytes)'
        >>,<< majf 4d
          'Number of page reclaims'
        >>,<< max-rss 8d
          'Maximum resident set size (in Kbytes)'
        >>,<< minf 4d
          'Number of page reclaims'
        >>,<< mrcv 4d
          'Number of messages received'
        >>,<< msnd 4d
          'Number of messages sent'
        >>,<< nsig 4d
          'Number of signals received'
        >>,<< nswp 4d
          'Number of swaps'
        >>,<< volcsw 6d
          'Number of voluntary context switches'
        >>,<< outb 4d
          'Number of block output operations'
        >>,<< util% 6.2f
          'Percentage of CPU utilization (0..100%)'
        >>,<< wallclock 9d
          'Number of microseconds elapsed'
        >>
    }

    method default-columns() { < wallclock util% max-rss > }

    method preamble($first, $last, $total, @snaps --> Str:D) {
        qq:to/HEADER/.chomp;
Initial/Final Size: { $first<max-rss> } / { $last<max-rss> } Kbytes
Total Time:      { ($total<wallclock> / 1000000).fmt('%9.2f') } seconds
Total CPU Usage: { ($total<cpu> / 1000000).fmt('%9.2f') } seconds
HEADER
    }

    # actual snapping logic
    class Snap does Telemetry::Instrument::Snap {

        # Helper stuff
        my int $start =
          nqp::fromnum_I(Rakudo::Internals.INITTIME * 1000000,Int);
        my int $cores = Kernel.cpu-cores;
        my $utilize   = 100 / $cores;
        my int $b2kb = VM.osname eq 'darwin' ?? 10 !! 0;

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
          max-rss ix-rss id-rss is-rss minf   majf   nswp inb
          outb    msnd   mrcv   nsig   volcsw invcsw wallclock
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
            (my int $wallclock = nqp::atpos_i(data,WALLCLOCK))
              ?? (nqp::atkey($dispatch,'cpu')(data) / $wallclock)
              !! $cores
        }
        %dispatch<util%> = -> Mu \data {
            $utilize * nqp::atkey($dispatch,'cpus')(data)
        }

        method AT-KEY(Str:D $key) {
            nqp::ifnull(
              nqp::atkey($dispatch,$key),
              -> Mu \data { Nil }
            )($!data)
        }

        method EXISTS-KEY(Str:D $key) {
            nqp::hllbool(nqp::existskey($dispatch,$key))
        }

        method !snap() is raw {
            nqp::stmts(
              (my int @data),
              (nqp::getrusage(@data)),
              nqp::bindpos_i(
                @data,
                WALLCLOCK,
                nqp::sub_i(nqp::div_i(nqp::time,1000),$start)
              ),
              @data
            )
        }
    }

    method snap(--> Snap:D) { Snap.new }
}

# Telemetry data of starting Threads -------------------------------------------
class Telemetry::Instrument::Thread does Telemetry::Instrument {

    method formats() is raw {
           << tad 3d
          'Number of threads that ended with an exception (aborted)'
        >>,<< tcd 3d
          'Number of threads that completed without any problem'
        >>,<< thid 4d
          'Highest OS thread ID seen'
        >>,<< tjd 3d
          'Number of threads that were joined'
        >>,<< tsd 3d
          'Number of threads that were started'
        >>,<< tys 4d
          'Number of times a thread was yielded'
        >>
    }

    method default-columns() { < tsd tcd tad thid > }

    method preamble($first, $last, $total, @snaps --> Str:D) {
        qq:to/HEADER/.chomp;
OS threads started: { ($last<thid> - $first<thid>).fmt('%4d') }{ " ($first<thid> started earlier)" if $first<thid> }
HEADER
    }

    # actual snapping logic
    class Snap does Telemetry::Instrument::Snap {

        # Initialize the dispatch hash using HLL features, as we only need to
        # do this on module load time.  Note that the order matters here!
        my %dispatch = <tsd tad tcd tjd tys thid>.kv.map: -> int $index, $name {
            $name => -> Mu \data { nqp::atpos_i(data,$index) }
        }

        # Allow for low-level dispatch hash access for speed
        my $dispatch := nqp::getattr(%dispatch,Map,'$!storage');

        method AT-KEY(Str:D $key) {
            nqp::ifnull(
              nqp::atkey($dispatch,$key),
              -> Mu \data { Nil }
            )($!data)
        }

        method EXISTS-KEY(Str:D $key) {
            nqp::hllbool(nqp::existskey($dispatch,$key))
        }

        method !snap() is raw { Thread.usage }
    }

    method snap(--> Snap:D) { Snap.new }
}

# Telemetry data from the ThreadPoolScheduler ----------------------------------
class Telemetry::Instrument::ThreadPool does Telemetry::Instrument {

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

    method default-columns() { < gw gtc tw ttc aw atc > }

    method preamble($first, $last, $total, @snaps --> Str:D) {
        my $text := nqp::list_s;
        if $first<s> {
            nqp::push_s($text,"Supervisor thread ran the whole time");
        }
        elsif !$last<s> {
            nqp::push_s($text,"No supervisor thread has been running");
        }
        else {
            my $started = @snaps.first: *.<s>;
            nqp::push_s($text,"Supervisor thread ran for {
              (100 * ($last<wallclock> - $started<wallclock>)
                / $total<wallclock>).fmt("%5.2f")
            }% of the time");
        }
        nqp::join("\n",$text)
    }

    # actual snapping logic
    class Snap does Telemetry::Instrument::Snap {

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

        method AT-KEY(Str:D $key) {
            nqp::ifnull(
              nqp::atkey($dispatch,$key),
              -> Mu \data { Nil }
            )($!data)
        }

        method EXISTS-KEY(Str:D $key) {
            nqp::hllbool(nqp::existskey($dispatch,$key))
        }

        method !snap() is raw {
            $*SCHEDULER ?? $*SCHEDULER.usage !! ThreadPoolScheduler.usage
        }
    }

    method snap(--> Snap:D) { Snap.new }
}

# Telemetry::Instrument::Adhoc -------------------------------------------------
class Telemetry::Instrument::AdHoc does Telemetry::Instrument {
    has @!formats;
    has @!columns;
    has Mu $!containers;
    has Mu $!dispatch;

    multi method new(::?CLASS: *@vars is raw, *%vars is raw) {
        nqp::create(self)!SET-SELF(@vars, %vars)
    }

    method !SET-SELF(\array, \hash) {
        $!containers := nqp::create(IterationBuffer);
        $!dispatch := nqp::create(Rakudo::Internals::IterationSet);

        for array {
            my int $index = nqp::elems($!containers);
            if nqp::istype($_,Pair) {
                my $variable := .value;
                die "Must specify a container" unless nqp::iscont($variable);

                my str $name = $variable.VAR.name.substr(1);
                @!formats.push([$name,"{4 max nqp::chars($name)}d",.key]);
                @!columns.push($name);
                nqp::bindpos($!containers,$index,$variable.VAR);
                nqp::bindkey($!dispatch,$name,
                  -> Mu \data { nqp::atpos_i(data,$index) });
            }
            else {
                die "Must specify a container" unless nqp::iscont($_);
                my str $name  = .VAR.name;
                @!formats.push([$name,"{4 max nqp::chars($name)}d",""]);
                @!columns.push($name);
                nqp::bindpos($!containers,$index,$_);
                nqp::bindkey($!dispatch,$name,
                  -> Mu \data { nqp::atpos_i(data,$index) });
            }
        }
        self
    }

    method preamble($first, $, $, @ --> Str:D) {
        my $text := nqp::list_s;
        for @!columns -> $name {
            nqp::push_s($text,
              "Initial $name.tc(): ".fmt('%-17s') ~ $first{$name}.fmt('%9d')
            );
        }
        nqp::join("\n",$text)
    }

    # actual snapping logic
    class Snap does Telemetry::Instrument::Snap {
        has Mu $!instrument;

        multi method new(::?CLASS: Telemetry::Instrument::AdHoc:D \instrument) {
            my $self := nqp::create(self);
            nqp::bindattr($self,::?CLASS,'$!instrument',instrument);
            nqp::p6bindattrinvres($self,::?CLASS,'$!data',$self!snap)
        }

        method AT-KEY(Str:D $key) {
            nqp::ifnull(
              nqp::atkey(
                nqp::getattr(
                  $!instrument,Telemetry::Instrument::AdHoc,'$!dispatch'),
                $key
              ),
              -> Mu \data { Nil }
            )($!data)
        }

        method EXISTS-KEY(Str:D $key) {
            nqp::hllbool(
              nqp::existskey(
                nqp::getattr(
                  $!instrument,Telemetry::Instrument::AdHoc,'$!dispatch'),
                $key
              )
            )
        }

        method !snap() {
            my $containers := nqp::getattr(
              $!instrument,Telemetry::Instrument::AdHoc,'$!containers');
            my int $i = -1;
            my int $elems = nqp::elems($containers);
            my $data := nqp::setelems(nqp::list_i,$elems);
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_i($data,$i,nqp::decont(nqp::atpos($containers,$i)))
            );
            $data
        }
    }

    method formats() { @!formats }
    method default-columns() { @!columns }
    method snap(--> Snap:D) { Snap.new(self) }
}

# Telemetry::Sampler -----------------------------------------------------------
class Telemetry::Sampler {
    has $!instruments;
    has $!dispatcher;
    has $!formats;

    # helper sub for handling instruments specified with a Str
    sub Str-instrument($name) {
        (my $class := nqp::decont(Telemetry::Instrument::{$name})) =:= Any
          ?? die "Could not find Telemetry::Instrument::$name class"
          !! $class
    }

    method !set-up-instrument($instrument is copy --> Nil) {
        my $class = nqp::istype($instrument,Str)
          ?? Str-instrument($instrument)
          !! $instrument;
        my int $index = nqp::elems($!instruments);
        $!instruments.push($class);

        my constant KEY    = 0;
        my constant FORMAT = 1;
        my constant LEGEND = 2;

        for $class.formats -> @info {
            my str $key = @info[KEY];
            nqp::bindkey($!dispatcher,$key, -> Mu \samples {
              nqp::atpos(samples,$index).AT-KEY($key)
            });
            nqp::bindkey($!formats,$key,@info);
        }
    }

    multi method new(Telemetry::Sampler:) { self.new([]) }
    multi method new(Telemetry::Sampler: Mu \instrument) {
        self.new(List.new(instrument))
    }
    multi method new(Telemetry::Sampler: @spec) {
        my $self := nqp::create(self);
        nqp::bindattr($self,self,'$!instruments',
          nqp::create(IterationBuffer));
        nqp::bindattr($self,self,'$!dispatcher',
          nqp::create(Rakudo::Internals::IterationSet));
        nqp::bindattr($self,self,'$!formats',
          nqp::create(Rakudo::Internals::IterationSet));

        # handle instrument specification
        if @spec {
            $self!set-up-instrument($_) for @spec;
        }

        # none specified, but we do have a default in the environment
        elsif %*ENV<RAKUDO_TELEMETRY_INSTRUMENTS> -> $rri {
            $self!set-up-instrument(Str-instrument($_))
              for $rri.comb( /<[\w-]>+/ );
        }

        # no instruments to be found anywhere, use the default default
        else {
            $self!set-up-instrument($_) for
              Telemetry::Instrument::Usage,
              Telemetry::Instrument::ThreadPool,
            ;
        }

        $self
    }

    method set-instruments(Telemetry::Sampler:D: *@instruments --> Nil) {
        nqp::bindattr(self,Telemetry::Sampler,'$!instruments',
          nqp::create(IterationBuffer));
        nqp::bindattr(self,Telemetry::Sampler,'$!dispatcher',
          nqp::create(Rakudo::Internals::IterationSet));
        nqp::bindattr(self,Telemetry::Sampler,'$!formats',
          nqp::create(Rakudo::Internals::IterationSet));

        self!set-up-instrument($_) for @instruments;
        $snaps := nqp::create(IterationBuffer);
    }

    multi method raku(Telemetry::Sampler:D: --> Str:D) {
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
INIT without $*SAMPLER {
    PROCESS::<$SAMPLER> := Telemetry::Sampler.new;
}

# Telemetry --------------------------------------------------------------------
class Telemetry does Associative {
    has     $!sampler;
    has     $!samples;
    has Str $.message is rw;

    multi method new(Telemetry:) {
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
          nqp::bindpos($samples,$i,nqp::atpos($instruments,$i).snap)
        );

        $self
    }
    multi method new(Telemetry: *@samples) { # needed for .raku roundtripping
        my $self := nqp::create(self);
        nqp::bindattr($self,Telemetry,'$!sampler',
          my $sampler := nqp::decont($*SAMPLER));

        my $samples  := nqp::create(IterationBuffer);
        my int $elems = +@samples;  # reify
        my $reified  := nqp::getattr(@samples,List,'$!reified');
        nqp::if($reified,nqp::splice($samples,$reified,0,$elems));

        nqp::p6bindattrinvres($self,Telemetry,'$!samples',$samples);
    }

    multi method raku(Telemetry:D: --> Str:D) {
        self.^name ~ ".new$!samples.List.raku()"
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

    method EXISTS-KEY($key) {
        nqp::hllbool(
          nqp::existskey(
            nqp::getattr($!sampler,Telemetry::Sampler,'$!dispatcher'),
            $key
          )
        )
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
    die "Cannot subtract Telemetry type objects";
}
multi sub infix:<->(
  Telemetry:D \a, Telemetry:U \b --> Telemetry::Period:D) is export {
    a - b.new
}
multi sub infix:<->(
  Telemetry:U \a, Telemetry:D \b --> Telemetry::Period:D) is export {
    a.new - b
}
multi sub infix:<->(
  Telemetry:D \a, Telemetry:D \b --> Telemetry::Period) is export {
    my $a := nqp::decont(a);
    my $b := nqp::decont(b);
    my $period := nqp::create(Telemetry::Period);
    nqp::bindattr($period,Telemetry,'$!sampler',
      nqp::getattr($a,Telemetry,'$!sampler'));
    $period.message = $_ with $a.message;

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
proto sub snap(|) is export {*}
multi sub snap(--> Nil) {
    $snaps.push(Telemetry.new);
}
multi sub snap(Str:D $message --> Nil) {
    my \T := Telemetry.new;
    T.message = $message;
    $snaps.push(T);
}
my $snapshot-idx = 1;
multi sub snap(Str $message = "taking heap snapshot...", :$heap! --> Nil) {
    my $filename =
        $heap eqv True
            ?? "heapsnapshot-$($*PID)-$($snapshot-idx++).mvmheap"
            !! $heap ~~ Str:D
                ?? $heap.IO.e
                    ?? "$heap-$($*PID)-$($snapshot-idx++).mvmheap"
                    !! $heap
                !! $heap ~~ IO::Path:D
                    ?? $heap.absolute
                    !! $heap eqv False
                        ?? do { $message eq "taking heap snapshot..."
                                    ?? snap()
                                    !! snap($message);
                                return }
                        !! die "heap argument to snap must be a Bool, Str, or IO::Path, not a $heap.WHAT()";

    my \T1 := Telemetry.new;
    T1.message = $message with $message;
    $snaps.push(T1);
    Perl6::Compiler.profiler-snapshot(kind => "heap", filename => $filename<>);
    my \T2 := Telemetry.new;
    T2.message = $filename;
    $snaps.push(T2);
}
multi sub snap(@s --> Nil) {
    @s.push(Telemetry.new);
}

# Starting the snapper / changing the period size
my int $snapper-running;
my $snapper-wait;
sub snapper($sleep = 0.1, :$stop, :$reset --> Nil) is export {

    $snapper-wait = $sleep;
    $snaps := nqp::create(IterationBuffer) if $reset;

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
    my $new := $snaps;
    $snaps := nqp::create(IterationBuffer);
    $new.push(Telemetry.new) if $new.elems == 1;
    periods(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$new));
}
multi sub periods(@s) { (1..^@s).map: { @s[$_] - @s[$_ - 1] } }

# Telemetry reporting features -------------------------------------------------
proto sub report(|) is export {*}
multi sub report(*%_ --> Str:D) {
    report(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$snaps),|%_)
}

# some constants for the %format list
my constant NAME    = 0; # short name
my constant FORMAT  = 1; # format (without % prefixed)
my constant LEGEND  = 2; # legend
my constant HEADER  = 3; # generated: column header
my constant FOOTER  = 4; # generated: column footer
my constant DISPLAY = 5; # generated: code to execute to display

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
  --> Str:D
) {

    # set up basic header
    my $text := nqp::list_s(qq:to/HEADER/.chomp);
Telemetry Report of Process #$*PID ({Instant.from-posix(nqp::div_i(nqp::time,1000000000)).DateTime})
Number of Snapshots: {+@s}
HEADER

    # return that if there's nothing to tell otherwise
    return nqp::atpos_s($text,0) unless @s;

    # get the sampler that was used
    my $sampler := @s[0].sampler;

    # determine columns to be displayed
    unless @columns {
        if %*ENV<RAKUDO_REPORT_COLUMNS> -> $rrc {
            @columns = $rrc.comb( /<[\w%-]>+/ );
        }
        else {
            @columns.append(.default-columns) for $sampler.instruments;
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
            (%format{$column}[NAME]
              or note "WARNING: Unknown Telemetry column `$column`."
                ~ " Perhaps you need to adjust used instruments"
                ~ " (see RAKUDO_TELEMETRY_INSTRUMENTS)\n"
              and 0
            ) and @periods.first: { .{%format{$column}[NAME]} }
        };
        my $header  = "\n%format{@columns}>>.[HEADER].join(' ')";
        my @formats = %format{@columns};

        for $sampler.instruments -> \instrument {
            nqp::push_s($text,$_)
              with instrument.preamble: $first, $last, $total, @s;
        }

        sub push-period($period --> Nil) {
            with $period.message -> $message {
                my $line = "#-- $message ";
                nqp::push_s($text,$line ~ "-" x 80 - $line.chars);
            }
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

sub T (--> Telemetry:D) is export { Telemetry.new }

# Provide limited export capability --------------------------------------------

sub EXPORT(*@args) {
    (EXPORT::DEFAULT::{ @args.map: '&' ~ * }:p).Map
}

# Make sure we tell the world if we're implicitely told to do so ---------------
END {
    $snapper-running = 0;  # stop any snapper
    if $snaps.elems {
        snap;
        note report;
    }
}

# vim: expandtab shiftwidth=4
