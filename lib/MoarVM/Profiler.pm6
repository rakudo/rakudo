# An object oriented interface to the data structure as returned by
# nqp::mvmendprofile, when started with mvmstartprofile({:instrumented})

# We need NQP here, duh!
use nqp;

# stubs we need
class MoarVM::Profiler::Thread { ... }

# some helper subs
sub infix:<%%%>(\a,\b --> Str:D) { sprintf "%.2f%%", (100 * a) / b }
sub infix:<avg>(\a,\b --> Str:D) { sprintf "(avg. %.2f)", a / b }
sub prefix:<§>(\a --> Str:D) { a == 1 ?? "" !! "s" }

# Simple role that maps a set of given keys onto a hash, so that we need to
# do the minimal amount of work to convert the data structure to a full-blown
# object hierarchy.
role OnHash[@keys] {
    has %.hash;

    # this gets run at mixin time, before the class is composed
    for @keys -> $key {
        $?CLASS.^add_method($key, { .hash.AT-KEY($key) } )
    }

    # don't want to use named parameters for something this simple
    method new(%hash) { self.bless(:%hash) }

    # make sure we have an object, and not just a hash for a given key
    method !mogrify-to-object(\the-class, \key, \link --> Nil) {
        if %!hash{key} -> $hash {
            unless $hash ~~ the-class {
                my \object := the-class.new($hash);
                %!hash.BIND-KEY(key,object);
                object.hash.BIND-KEY(link,self);
            }
        }
        else {
            %!hash.BIND-KEY(key,the-class.new({}));
        }
    }

    # make sure we have a Slip of objects, and not just an array of hashes
    method !mogrify-to-slip(\the-class, \key, \link --> Nil) {
        if %!hash{key} -> @list {
            %!hash.BIND-KEY(
              key,
              @list.map({
                  if $_ ~~ the-class {
                      $_
                  }
                  else {
                      my \object := the-class.new($_);
                      object.hash.BIND-KEY(link,self);
                      object
                  }
              }).Slip
            );
        }
        else {
            %!hash.BIND-KEY(key,Empty);
        }
    }

    method Str(--> Str:D) { self.gist }
}

# Information about objects of a certain type being allocated in a Callee.
class MoarVM::Profiler::Allocation does OnHash[<
  callee
  count
  id
  jit
>] {

    method TWEAK(--> Nil) {
        %!hash.BIND-KEY('jit',0) unless %!hash<jit>;
    }
    
    method thread() { self.callee.thread }
    method name()   { self.thread.type_by_id($.id).name }
    method file()   { self.callee.file }
    method line()   { self.callee.line }

    method gist(--> Str:D) {
        my $gist = "Allocated $.count objects of $.name";
        $gist ~= " (JITted $.jit)" if $.jit;
        $gist ~ "\n  at $.file line $.line"
    }
}

# Information about a Callable that has been called at least once.
class MoarVM::Profiler::Callee does OnHash[<
  allocations
  callees
  caller
  entries
  exclusive_time
  file
  first_entry_time
  id
  inclusive_time
  inlined_entries
  jit_entries
  line
  name
  osr
>] {
    has $!nr_allocations;
    has $!nr_exclusive_allocations;
    has $!nr_frames;
    has $!nr_inlined;
    has $!nr_jitted;
    has $!nr_osred;

    method TWEAK(--> Nil) {
        self!mogrify-to-slip(
          MoarVM::Profiler::Allocation, 'allocations', 'callee');
        self!mogrify-to-slip(
          MoarVM::Profiler::Callee, 'callees', 'caller');

        %!hash<name>            //= "";
        %!hash<entries>         //= 0;
        %!hash<inlined_entries> //= 0;
        %!hash<jit_entries>     //= 0;
        %!hash<osr>             //= 0;
    }

    method all_callees() {
        self.callees, |self.callees.map: |*.all_callees
    }
    method all_allocations() {
        self.allocations, |self.callees.map: |*.all_allocations
    }

    method nr_callees() {
        self.callees.elems
    }
    method nr_exclusive_allocations(--> Int:D) {
        $!nr_exclusive_allocations //=
          self.allocations.map(*.count).sum
    }
    method nr_allocations(--> Int:D) {
        $!nr_allocations //=
          self.nr_exclusive_allocations + self.callees.map(*.nr_allocations).sum
    }
    method nr_frames(--> Int:D) {
       $!nr_frames //=
         self.entries + self.callees.map(*.nr_frames).sum
    }
    method nr_inlined(--> Int:D) {
        $!nr_inlined //=
          self.inlined_entries + self.callees.map(*.nr_inlined).sum
    }
    method nr_jitted(--> Int:D) {
        $!nr_jitted //=
          self.jit_entries + self.callees.map(*.nr_jitted).sum
    }
    method nr_osred(--> Int:D) {
        $!nr_osred //= self.osr + self.callees.map(*.nr_osred).sum
    }

    method average_inclusive_time(--> Str:D) {
        sprintf("%.2f",$.inclusive_time / $.nr_frames)
    }
    method average_exclusive_time(--> Str:D) {
        sprintf("%.2f",$.exclusive_time / $.nr_frames)
    }

    method thread() {
        my $thread = self.caller;
        $thread = $thread.caller until $thread ~~ MoarVM::Profiler::Thread;
        $thread
    }

    method gist(--> Str:D) {
        if $.entries -> $entries {
            my $gist = $.name ?? "'$.name'" !! 'Callee';
            $gist ~= " was called $_ time{§$_}\n" given $entries;
            $gist ~= "   at $.file line {$.line}\n";

            $gist ~= $_ ~~ MoarVM::Profiler::Thread
              ?? " from thread #" ~ .id ~ ".\n"
              !! " from " ~ .file ~ " line " ~ .line ~ ".\n"
              given self.caller;

            $gist ~= "$_ call{ $_ == 1 ?? " was" !! "s were"} inlined ({ $_ %%% $entries }).\n"
              if $_ given $.inlined_entries;
            $gist ~= "$_ call{ $_ == 1 ?? " was" !! "s were"} jitted ({ $_ %%% $entries }).\n"
              if $_ given $.jit_entries;

            if $entries > 1 {
                $gist ~= "First called at $.first_entry_time microsecs for $_ microsecs { $_ avg $entries }"
                  given $.inclusive_time;
            }
            else {
                $gist ~= "Called at $.first_entry_time microsecs for $.inclusive_time microsecs";
            }

            if $.callees {
                $gist ~= "\n  of which $_ microsecs here { $_ avg $entries }.\n"
                given $.exclusive_time;
            }
            else {
                $gist ~= ".\n";
            }

            if $.nr_allocations -> $allocations {
                $gist ~= "Did $_ allocation{§$_}" given $allocations;
                $gist ~= $allocations == $_
                  ?? ".\n"
                  !! " (of which $_ ({ $_ %%% $allocations }) { $_ == 1 ?? "was" !! "were"} done here).\n"
                  given $.nr_exclusive_allocations;
            }
            $gist ~= "Had $_ On Stack Replacement{§$_}.\n"
              if $_ given $.osr;
            $gist
        }
        else {
            "Callee without information"
        }
    }
}

# Information about a de-allocation as part of a garbage collection.
class MoarVM::Profiler::Deallocation does OnHash[<
  id
  gc
  nursery_fresh
  nursery_seen
>] {
    has Str $.name;

    method name(--> Str:D) {
        $!name //= self.gc.thread.type_by_id($.id).name
    }
    method gist(--> Str:D) {
        "De-allocation of $.name in garbage collection {$.gc.sequence}"
    }
}


# Information about a garbage collection.
class MoarVM::Profiler::GC does OnHash[<
  cleared_bytes
  deallocs
  full
  gen2_roots
  promoted_bytes
  promoted_bytes_unmanaged
  responsible
  retained_bytes
  sequence
  start_time
  thread
  time
>] {

    method TWEAK(--> Nil) {
        self!mogrify-to-slip(MoarVM::Profiler::Deallocation, 'deallocs', 'gc');
    }

    method gist(--> Str:D) {
        "Garbage collection $.sequence cleared $.cleared_bytes bytes"
    }
}

# Information about a type that have at least one object instantiated.
class MoarVM::Profiler::Type does OnHash[<
  managed_size
  repr
  type
  has_unmanaged_data
>] {
    has Int $.id;
    has Str $.name;
    has %.threads;  # set by MoarVM::Profiler.new

    method new( ($id,%hash) ) { self.bless(:$id, :%hash) }

    method name() {
        $!name //= (try .^name) || "(" ~ nqp::objectid($_) ~ ")"
          given %!hash<type>;
    }

    # thread given ID
    method thread($id) { %!threads{$id} }

    method gist(--> Str:D) {
        "Type '$.name' of REPR '$.repr' ($.managed_size bytes)"
    }
}

# Information about a thread.
class MoarVM::Profiler::Thread does OnHash[<
  gcs
  parent
  spesh_time
  start_time
  total_time
>] {
    has Int $.id;
    has $.callee;
    has %.types;  # set by MoarVM::Profiler.new

    method TWEAK(--> Nil) {
        self!mogrify-to-object(MoarVM::Profiler::Callee,'call_graph','caller');
        self!mogrify-to-slip(MoarVM::Profiler::GC,'gcs','thread');

        # some corrections
        $!id     := %!hash.DELETE-KEY("thread");
        $!callee := %!hash.DELETE-KEY("call_graph");
    }

    # type given ID
    method type_by_id($id)     { %!types{$id}   }
    method type_by_name($name) { %.names{$name} }

    method all_callees()     { self.callee, |self.callee.all_callees }
    method all_allocations() { self.callee.all_allocations }

    method nr_allocations(--> Int:D) { self.callee.nr_allocations  }
    method nr_frames(--> Int:D)      { self.callee.nr_frames       }
    method nr_inlined(--> Int:D)     { self.callee.nr_inlined      }
    method nr_jitted(--> Int:D)      { self.callee.nr_jitted       }
    method nr_osred(--> Int:D)       { self.callee.nr_osred        }
    method nr_gcs(--> Int:D)         { self.gcs.elems              }
    method nr_full_gcs(--> Int:D)    { self.gcs.grep(*.full).elems }

    method callees_by_file(\matcher) {
        self.all_callees.grep({ matcher.ACCEPTS(.file) })
    }
    method allocations_by_file(\matcher) {
        self.callees_by_file(matcher).map: *.allocations
    }

    method gist(--> Str:D) {
        my $gist = "Thread #{$.id}{ " (from thread #$.parent)" if $.parent}:\n";
        $gist ~= $_ ?? "Started at $_ microseconds and r" !! "R"
          given $.start_time;
        $gist ~= "an for $.total_time microseconds";
        $gist ~= $_ ?? " (of which $_ in spesh).\n" !! ".\n"
          given $.spesh_time;
        if $.nr_gcs -> $gcs {
            $gist ~= "Did $gcs garbage collections";
            $gist ~= $_ ?? " (of which $_ full collections).\n" !! ".\n"
              given $.nr_full_gcs;
        }
        if $.nr_frames -> $frames {
            $gist ~= "Called $frames frames.\n";
            $gist ~= "$_ frames were inlined ({$_ %%% $frames}).\n"
              given $.nr_inlined;
            $gist ~= "$_ frames were jitted ({$_ %%% $frames}).\n"
              given $.nr_jitted;
            $gist ~= "$_ On Stack Replacement{§$_} were done.\n"
              given $.nr_osred;
        }
        else {
            $gist ~= "No profileable code was executed.";
        }

        $gist
    }
}

# Main object returned by profile() and friends.
class MoarVM::Profiler {
    has %.types   is required;
    has %.threads is required;
    has %.names;

    method !SET-SELF(@raw) {
        %!types = @raw[0].map: -> $type {
            .id => $_ given MoarVM::Profiler::Type.new($type)
        }
        %!threads = @raw.skip.map: -> $thread {
            .id => $_ given MoarVM::Profiler::Thread.new($thread)
        }

        # let types know about threads and vice-versa
        nqp::bindattr(
          nqp::decont($_),MoarVM::Profiler::Type,'%!threads',%!threads
        ) for %!types.values;
        nqp::bindattr(
          nqp::decont($_),MoarVM::Profiler::Thread,'%!types',%!types
        ) for %!threads.values;

        self
    }
    method new(@raw) { self.CREATE!SET-SELF(@raw) }

    method names() {
        %!names
          ?? %!names
          !! %!names = %.types.map( { .type => $_ with .value } )
    }

    # type/thread given an ID
    method type_by_id($id)     { %!types{$id}   }
    method type_by_name($name) { %.names{$name} }
    method thread($id)         { %!threads{$id} }

    method all_callees()     { %!threads.values.map: |*.all_callees     }
    method all_allocations() { %!threads.values.map: |*.all_allocations }

    method my_callees() {
        self.callees_by_file(callframe(1).file)
    }
    method callees_by_file(\matcher) {
        self.all_callees.grep({ matcher.ACCEPTS(.file) })
    }

    method my_allocations() {
        self.allocations_by_file(callframe(1).file)
    }
    method allocations_by_file(\matcher) {
        self.callees_by_file(matcher).map: *.allocations
    }

    method report(--> Str:D) {
        (
"  #   wallclock   objects    frames   inlined    jitted   OSR   GCs",
"----+-----------+---------+---------+---------+---------+-----+-----",
          |self.threads.grep(*.value.nr_frames).sort(*.key).map( {
              sprintf("%3d %11d %9d %9d %9d %9d %5d %5d",
                .id,
                .total_time,
                .nr_allocations,
                .nr_frames,
                .nr_inlined,
                .nr_jitted,
                .nr_osred,
                .nr_gcs,
              ) given .value
          } ),
"----+-----------+---------+---------+---------+---------+-----+-----",
        ).join("\n")
    }

    method gist(--> Str:D) {
        self.threads.sort(*.key).map(*.value.gist).join("-" x 80 ~ "\n")
    }
    method Str(--> Str:D) { self.gist }

    method sink(--> Nil) { note self }

    method profile(&code --> MoarVM::Profiler:D) {
        nqp::mvmstartprofile({:instrumented});
        code();
        MoarVM::Profiler.new(nqp::mvmendprofile)
    }

    method average(MoarVM::Profiler:U: *@profiles) {

    }

    method average_profile(&code, :$times = 5) {
        self.WHAT.average( (^$times).map: self.profile(&code) )
    }
}

# Raw subs, for cases where starting an extra scope would be troublesome
sub profile_start(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented})
}

sub profile_end(--> MoarVM::Profiler:D) is export {
    MoarVM::Profiler.new(nqp::mvmendprofile)
}

# Profile the rest of the compilation unit
sub profile_rest(--> Nil) is export {
    nqp::mvmstartprofile({:instrumented});
    my $end-profile = True;
    END MoarVM::Profiler.new(nqp::mvmendprofile) if $end-profile;
}

# HLL sub for profiling a piece of code and getting the info from that
sub profile(&code --> MoarVM::Profiler:D) is export {
    MoarVM::Profiler.profile(&code)
}
